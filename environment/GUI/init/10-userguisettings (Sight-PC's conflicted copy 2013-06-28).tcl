# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1920 || [winfo screenheight .] != 1080 || [lindex [wm maxsize .] 0] != 1916 || [lindex [wm maxsize .] 1] != 1054} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.bufferstatus) 491x200+1238+179
  set changed_window_list(.bufferstatus) 1
  set window_config(.stepper) 500x550+364+204
  set changed_window_list(.stepper) 1
  set window_config(.visicon) 850x150+473+272
  set changed_window_list(.visicon) 1
  set window_config(.retrieval_history) 200x200+860+440
  set changed_window_list(.retrieval_history) 1
  set window_config(.control_panel) 167x722+1646+0
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 420x300+1152+589
  set changed_window_list(.declarative) 1
  set window_config(.reload_response) 500x228+710+425
  set changed_window_list(.reload_response) 1
  set window_config(.procedural) 484x400+1231+343
  set changed_window_list(.procedural) 1
  set window_config(.whynot) 200x300+860+390
  set changed_window_list(.whynot) 1
  set window_config(.buffers) 350x240+1002+482
  set changed_window_list(.buffers) 1
}
