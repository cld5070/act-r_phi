# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
update
wm withdraw .
if {[winfo screenwidth .] != 3440 || [winfo screenheight .] != 1440 || [lindex [wm maxsize .] 0] != 3444 || [lindex [wm maxsize .] 1] != 1421} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.bufferstatus) 450x240+1223+277
  set changed_window_list(.bufferstatus) 1
  set window_config(.audicon) 870x150+1285+620
  set changed_window_list(.audicon) 1
  set window_config(.visicon) 1460x689+841+63
  set changed_window_list(.visicon) 1
  set window_config(.control_panel) 235x700+1770+71
  set changed_window_list(.control_panel) 1
  set window_config(.options) 450x274+1495+583
  set changed_window_list(.options) 1
  set window_config(.whynot) 956x300+1620+570
  set changed_window_list(.whynot) 1
  set window_config(.procedural) 1469x450+1103+147
  set changed_window_list(.procedural) 1
  set window_config(.buffers) 470x240+1718+702
  set changed_window_list(.buffers) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
