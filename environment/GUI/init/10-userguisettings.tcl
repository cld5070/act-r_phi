# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 1920 || [winfo screenheight .] != 1080 || [lindex [wm maxsize .] 0] != 1924 || [lindex [wm maxsize .] 1] != 1055} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.bufferstatus) 554x278+1270+771
  set changed_window_list(.bufferstatus) 1
  set window_config(.visicon) 1460x359+192+455
  set changed_window_list(.visicon) 1
  set window_config(.audicon) 870x148+845+620
  set changed_window_list(.audicon) 1
  set window_config(.control_panel) 317x700+840+291
  set changed_window_list(.control_panel) 1
  set window_config(.declarative) 854x300+1070+570
  set changed_window_list(.declarative) 1
  set window_config(.whynot) 692x300+827+349
  set changed_window_list(.whynot) 1
  set window_config(.buffers) 763x240+551+347
  set changed_window_list(.buffers) 1
  set window_config(.procedural) 1098x400+645+231
  set changed_window_list(.procedural) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
