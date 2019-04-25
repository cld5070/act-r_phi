# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 2560 || [winfo screenheight .] != 1440 || [lindex [wm maxsize .] 0] != 2564 || [lindex [wm maxsize .] 1] != 1415} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.audicon) 870x150+845+620
  set changed_window_list(.audicon) 1
  set window_config(.visicon) 1345x652+897+290
  set changed_window_list(.visicon) 1
  set window_config(.control_panel) 235x700+1747+376
  set changed_window_list(.control_panel) 1
  set window_config(.buffers) 910x215+1105+600
  set changed_window_list(.buffers) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
