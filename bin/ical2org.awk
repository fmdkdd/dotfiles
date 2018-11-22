#!/usr/bin/awk -f
# awk script for converting an iCal formatted file to a sequence of org-mode headings.
# this may not work in general but seems to work for day and timed events from Google's
# calendar, which is really all I need right now...
#
# usage:
#   awk -f THISFILE < icalinputfile.ics > orgmodeentries.org
#
# Note: change org meta information generated below for author and
# email entries!
#
# Caveats:
#
# - date entries with no time specified are assumed to be local time zone;
#   same remark for date entries that do have a time but do not end with Z
#   e.g.: 20130101T123456 is local and will be kept as 2013-01-01 12:34
#   where 20130223T123422Z is UTC and will be corrected appropriately
#
# - UTC times are changed into local times, using the time zone of the
#   computer that runs the script; it would be very hard in an awk script
#   to respect the time zone of a file belonging to another time zone:
#   the offsets will be different as well as the switchover time(s);
#   (consider a remote shell to a computer with the file's time zone)
#
# - the UTC conversion entirely relies on the built-in strftime method;
#   the author is not responsible for any erroneous conversions nor the
#   consequence of such conversions
#
# - does process RRULE recurring events, but ignores COUNT specifiers
#
# - does not process EXDATE to exclude date(s) from recurring events
#
# Eric S Fraga
# 20100629 - initial version
# 20100708 - added end times to timed events
#          - adjust times according to time zone information
#          - fixed incorrect transfer for entries with ":" embedded within the text
#          - added support for multi-line summary entries (which become headlines)
# 20100709 - incorporated time zone identification
#          - fixed processing of continuation lines as Google seems to
#            have changed, in the last day, the number of spaces at
#            the start of the line for each continuation...
#          - remove backslashes used to protect commas in iCal text entries
# no further revision log after this as the file was moved into a git
# repository...
#
# Updated by: Guido Van Hoecke <guivhoATgmailDOTcom>
# Last change: 2013.05.26 14:28:33
#----------------------------------------------------------------------------------

BEGIN {
    ### config section

    # maximum age in days for entries to be output: set this to -1 to
    # get all entries or to N>0 to only get enties that start or end
    # less than N days ago
    max_age = -1;

    # set to 1 or 0 to yes or not output a header block with TITLE,
    # AUTHOR, EMAIL etc...
    header = 0;

    # set to 1 or 0 to yes or not output the original ical preamble as
    # comment
    preamble = 0;

    # set to 1 to output time and summary as one line starting with
    # the time (value 1) or to 0 to output the summary as first line
    # and the date and time info as a second line
    condense = 0;

    # set to 1 or 0 to yes or not output the original ical entry as a
    # comment (mostly useful for debugging purposes)
    original = 1;

    # google truncates long subjects with ... which is misleading in
    # an org file: it gives the unfortunate impression that an
    # expanded entry is still collapsed; value 1 will trim those
    # ... and value 0 doesn't touch them
    trimdots = 1;

    # change this to your name
    author = "Eric S Fraga"

    # and to your email address
    emailaddress = "e.fraga@ucl.ac.uk"

    ### end config section

    # use a colon to separate the type of data line from the actual contents
    FS = ":";

    # we only need to preserve the original entry lines if either the
    # preamble or original options are true
    preserve = preamble || original
    first = 1;      # true until an event has been found
    max_age_seconds = max_age*24*60*60

    if (header) {
        print "#+TITLE:       Main Google calendar entries"
        print "#+AUTHOR:     ", author
        print "#+EMAIL:      ", emailaddress
        print "#+DESCRIPTION: converted using the ical2org awk script"
        print "#+CATEGORY:    google"
        print "#+STARTUP:     hidestars"
        print "#+STARTUP:     overview"
        print ""
    }
}

# continuation lines (at least from Google) start with a space
# if the continuation is after a description or a summary, append the entry
# to the respective variable

/^[ ]/ {
    if (indescription) {
        entry = entry gensub("\r", "", "g", gensub("^[ ]", "", "", $0));
    } else if (insummary) {
        summary = summary gensub("\r", "", "g", gensub("^[ ]", "", "", $0))
    }
    if (preserve)
        icalentry = icalentry "\n" $0
}

/^BEGIN:VEVENT/ {
    # start of an event: initialize global velues used for each event
    date = "";
    entry = ""
    headline = ""
    icalentry = ""  # the full entry for inspection
    id = ""
    indescription = 0;
    insummary = 0
    intfreq = "" # the interval and frequency for repeating org timestamps
    lasttimestamp = -1;
    location = ""
    rrend = ""
    status = ""
    summary = ""

    # if this is the first event, output the preamble from the iCal file
    if (first) {
        if(preamble) {
            print "* COMMENT original iCal preamble"
            print gensub("\r", "", "g", icalentry)
        }
        if (preserve)
            icalentry = ""
        first = false;
    }
}

# any line that starts at the left with a non-space character is a new data field

/^[A-Z]/ {
    # we do not copy DTSTAMP lines as they change every time you download
    # the iCal format file which leads to a change in the converted
    # org file as I output the original input.  This change, which is
    # really content free, makes a revision control system update the
    # repository and confuses.
    if (preserve)
        if (! index("DTSTAMP", $1))
            icalentry = icalentry "\n" $0
    # this line terminates the collection of description and summary entries
    indescription = 0;
    insummary = 0;
}

# this type of entry represents a day entry, not timed, with date stamp YYYYMMDD

/^DTSTART;VALUE=DATE/ {
    date = datestring($2);
}

/^DTEND;VALUE=DATE/ {
    time2 = datestring($2, 1);
    if ( issameday )
        time2 = ""
}

# this represents a timed entry with date and time stamp YYYYMMDDTHHMMSS
# we ignore the seconds

/^DTSTART[:;][^V]/ {
    date = datetimestring($2);
    # print date;
}

# and the same for the end date;

/^DTEND[:;][^V]/ {
    time2 = datetimestring($2);
    if (substr(date,1,10) == substr(time2,1,10)) {
        # timespan within same date, use one date with a time range
        date = date "-" substr(time2, length(time2)-4)
        time2 = ""
    }
}

# repetition rule

/^RRULE:FREQ=(DAILY|WEEKLY|MONTHLY|YEARLY)/ {
    # get the d, w, m or y value
    freq = tolower(gensub(/.*FREQ=(.).*/, "\\1", $0))
    # get the interval, and use 1 if none specified
    interval =  $2 ~ /INTERVAL=/ ? gensub(/.*INTERVAL=([0-9]+);.*/, "\\1", $2) : 1
    # get the enddate of the rule and use "" if none specified
    rrend = $2 ~ /UNTIL=/ ? datestring(gensub(/.*UNTIL=([0-9]{8}).*/, "\\1", $2)) : ""
    # build the repetitor vale as understood by org
    intfreq =  " +" interval freq
    # if the repetition is daily, and there is an end date, drop the repetitor
    # as that is the default
    if (intfreq == " +1d" && time2 =="" && rrend != "")
        intfreq = ""
}

# The description will the contents of the entry in org-mode.
# this line may be continued.

/^DESCRIPTION/ {
    $1 = "";
    entry = entry gensub("\r", "", "g", $0);
    indescription = 1;
}

# the summary will be the org heading

/^SUMMARY/ {
    $1 = "";
    summary = gensub("\r", "", "g", $0);

    # trim trailing dots if requested by config option
    if(trimdots && summary ~ /\.\.\.$/)
        sub(/\.\.\.$/, "", summary)
    insummary = 1;
}

# the unique ID will be stored as a property of the entry

/^UID/ {
    id = gensub("\r", "", "g", $2);
}

/^LOCATION/ {
    location = gensub("\r", "", "g", $2);
}

/^STATUS/ {
    status = gensub("\r", "", "g", $2);
}

# when we reach the end of the event line, we output everything we
# have collected so far, creating a top level org headline with the
# date/time stamp, unique ID property and the contents, if any

/^END:VEVENT/ {
    #output event
    if(max_age<0 || ( lasttimestamp>0 && systime()<lasttimestamp+max_age_seconds ) )
    {
        # build org timestamp
        if (intfreq != "")
            date = date intfreq
        if (time2 != "")
            date = date ">--<" time2
        else if (rrend != "")
            date = date ">--<" rrend

        # translate \n sequences to actual newlines and unprotect commas (,)
        if (condense)
            print "* <" date "> " gensub("^[ ]+", "", "", gensub("\\\\,", ",", "g", gensub("\\\\n", " ", "g", summary)))
        else
            print "* " gensub("^[ ]+", "", "", gensub("\\\\,", ",", "g", gensub("\\\\n", " ", "g", summary)))
        print ":PROPERTIES:"
        print     ":ID:       " id
        if(length(location))
            print ":LOCATION: " location
        if(length(status))
            print ":STATUS:   " status
        print ":END:"
        if (! condense)
            print "<" date ">"
        print ""
        # translate \n sequences to actual newlines and unprotect commas (,)
        if(length(entry)>1)
            print gensub("^[ ]+", "", "", gensub("\\\\,", ",", "g", gensub("\\\\n", "\n", "g", entry)));

        # output original entry if requested by 'original' config option
        if (original)
            print "** COMMENT original iCal entry\n", gensub("\r", "", "g", icalentry)
    }
}



# funtion to convert an iCal time string 'yyyymmddThhmmss[Z]' into a
# date time string as used by org, preferably including the short day
# of week: 'yyyy-mm-dd day hh:mm' or 'yyyy-mm-dd hh:mm' if we cannot
# define the day of the week

function datetimestring(input)
{
    # print "________"
    # print "input : " input
    # convert the iCal Date+Time entry to a format that mktime can understand
    spec  = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])T([0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1 \\2 \\3 \\4 \\5 \\6", "g", input);
    # print "spec :" spec

    stamp = mktime(spec);
    lasttimestamp = stamp;

    if (stamp <= 0) {
        # this is a date before the start of the epoch, so we cannot
        # use strftime and will deliver a 'yyyy-mm-dd hh:mm' string
        # without day of week; this assumes local time, and does not
        # attempt UTC offset correction
        spec = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])T([0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1-\\2-\\3 \\4:\\5", "g", input);
        # print "==> spec:" spec;
        return spec;
    }

    if (input ~ /[0-9]{8}T[0-9]{6}Z/ ) {
        # this is an utc time;
        # we need to correct the timestamp by the utc offset for this time
        offset = strftime("%z", stamp)
        pm = substr(offset,1,1) 1 # define multiplier +1 or -1
        hh = substr(offset,2,2) * 3600 * pm
        mm = substr(offset,4,2) * 60 * pm

        # adjust the timestamp
        stamp = stamp + hh + mm
    }

    return strftime("%Y-%m-%d %a %H:%M", stamp);
}

# function to convert an iCal date into an org date;
# the optional parameter indicates whether this is an end date;
# for single or multiple whole day events, the end date given by
# iCal is the date of the first day after the event;
# if the optional 'isenddate' parameter is non zero, this function
# tries to reduce the given date by one day

function datestring(input, isenddate)
{
    #convert the iCal string to a an mktime input string
    spec = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1 \\2 \\3 00 00 00", "g", input);

    # compute the nr of seconds after or before the epoch
    # dates before the epoch will have a negative timestamp
    # days after the epoch will have a positive timestamp
    stamp = mktime(spec);

    if (isenddate) {
        # subtract 1 day from the timestamp
        # note that this also works for dates before the epoch
        stamp = stamp - 86400;

        # register whether the end date is same as the start date
        issameday = lasttimestamp == stamp
    }
    # save timestamp to allow for check of max_age
    lasttimestamp = stamp

    if (stamp < 0) {
        # this date is before the epoch;
        # the returned datestring will not have the short day of week string
        # as strftime does not handle negative times;
        # we have to construct the datestring directly from the input
        if (isenddate) {
            # we really should return the date before the input date, but strftime
            # does not work with negative timestamp values; so we can not use it
            # to obtain the string representation of the corrected timestamp;
            # we have to return the date specified in the iCal input and we
            # add time 00:00 to clarify this
            return spec = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1-\\2-\\3 00:00", "g", input);
        } else {
            # just generate the desired representation of the input date, without time;
            return gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1-\\2-\\3", "g", input);
        }
    }

    # return the date and day of week
    return strftime("%Y-%m-%d %a", stamp);
}

# Local Variables:
# time-stamp-line-limit: 1000
# time-stamp-format: "%04y.%02m.%02d %02H:%02M:%02S"
# time-stamp-active: t
# time-stamp-start: "Last change:[ \t]+"
# time-stamp-end: "$"
# End:
