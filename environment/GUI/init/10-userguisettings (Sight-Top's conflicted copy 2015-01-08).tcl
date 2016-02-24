# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1600 || [winfo screenheight .] != 900 || [lindex [wm maxsize .] 0] != 1600 || [lindex [wm maxsize .] 1] != 877} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.control_panel) 170x700+1410+100
  set changed_window_list(.control_panel) 1
  set window_config(.reload_response) 500x230+550+335
  set changed_window_list(.reload_response) 1
}
