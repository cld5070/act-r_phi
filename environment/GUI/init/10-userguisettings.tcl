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
  set window_config(.buffer_history) 530x290+553+529
  set changed_window_list(.buffer_history) 1
  set window_config(.visicon) 1020x361+1181+411
  set changed_window_list(.visicon) 1
  set window_config(.options) 450x274+1055+583
  set changed_window_list(.options) 1
  set window_config(.buffers) 350x240+1649+302
  set changed_window_list(.buffers) 1
  set window_config(.bold_brain_3d_real) 658x508+951+470
  set changed_window_list(.bold_brain_3d_real) 1
  set window_config(.control_panel) 297x607+1735+267
  set changed_window_list(.control_panel) 1
  set window_config(.bold_brain_3d) 658x586+951+460
  set changed_window_list(.bold_brain_3d) 1
  set window_config(.audicon) 870x150+525+440
  set changed_window_list(.audicon) 1
  set window_config(.bufferstatus) 350x240+1105+600
  set changed_window_list(.bufferstatus) 1
  set window_config(.whynot) 200x300+1180+570
  set changed_window_list(.whynot) 1
  set window_config(.procedural) 500x400+773+391
  set changed_window_list(.procedural) 1
  set window_config(.copyright) 400x466+1080+487
  set changed_window_list(.copyright) 1
}
