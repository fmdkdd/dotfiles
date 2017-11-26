function fish_prompt
        # Save $status because set_color overrides it
        set -l last_status $status
        if [ $last_status -ne 0 ]
                set_color $fish_color_error
                echo -n "($last_status) "
        end
        set_color normal
        echo -n "$USER@"(hostname -s)
        set_color $fish_color_cwd
        echo -n " "(prompt_pwd)
        echo -n '> '
end
