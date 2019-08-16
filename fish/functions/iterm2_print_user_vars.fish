function iterm2_print_user_vars
    iterm2_set_user_var nodeVersion (node -v)
    iterm2_set_user_var pythonVersion (python3 --version)
    iterm2_set_user_var host ([ "$hostname" = "aliquote.home" ] && echo (ipconfig getifaddr en0) || echo "⇄ "(hostname -s))
end

