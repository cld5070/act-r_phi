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
  set window_config(.pgraph) 1581x400+1370+520
  set changed_window_list(.pgraph) 1
  set window_config(.dispatcher) 700x274+1370+583
  set changed_window_list(.dispatcher) 1
  set window_config(.pick_buffers) 200x340+1620+550
  set changed_window_list(.pick_buffers) 1
  set window_config(.control_panel) 235x700+3185+370
  set changed_window_list(.control_panel) 1
  set window_config(.options) 450x274+1495+583
  set changed_window_list(.options) 1
  set window_config(.text_trace_history) 714x340+1333+550
  set changed_window_list(.text_trace_history) 1
  set window_config(.procedural) 500x400+1470+520
  set changed_window_list(.procedural) 1
  set window_config(.bold_graphs) 660x250+1390+600
  set changed_window_list(.bold_graphs) 1
}
set gui_options(p_selected) #44DA22
set gui_options(p_matched) #FCA31D
set gui_options(p_mismatched) #E1031E
