
proc make_blending_history_viewer {} {
   
  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Blending History" -message "Blending history viewer requires a current model."
  } else {

    set win [toplevel [new_variable_name .blending_history]]
  
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .blending_history $win]
  
    # frame and list box for times

    set list_frame_1 [frame $win.list_frame_1 -borderwidth 0]  
  
    set list_box_1 [listbox $list_frame_1.list_box -listvar \
                            $list_frame_1.list_box.var \
                            -yscrollcommand "$list_frame_1.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

  
    set list_scroll_bar_1 [scrollbar $list_frame_1.list_scrl -command "$list_box_1 yview"]

    # Frame and list box for chunks
 
    set list_frame_2 [frame $win.list_frame -borderwidth 0]  
  
    set list_box_2 [listbox $list_frame_2.list_box -listvar \
                            $list_frame_2.list_box.var \
                            -yscrollcommand "$list_frame_2.list_scrl set" \
                            -selectmode single \
                            -exportselection 0 -font list_font -bd 0]

  
    set list_scroll_bar_2 [scrollbar $list_frame_2.list_scrl -command "$list_box_2 yview"]


    # The lables for the sections 

    set l1 [label $win.l1 -text "Times" -justify left -font label_font]
    set l2 [label $win.l2 -text "Blended Chunks" -justify left -font label_font]
    set l3 [label $win.l3 -text "Chunk" -justify left -font label_font]
    set l4 [label $win.l4 -text "Request" -justify left -font label_font]
    set l5 [label $win.l5 -text "Trace" -justify left -font label_font]
    set l6 [label $win.l6 -text "Result" -justify left -font label_font]

    set l0 [label $win.l0 -text "" -font label_font -textvariable $win.l0.value]

    # frame for the chunk display

    set text_frame_1 [frame $win.text_frame_1 -borderwidth 0]  
 
    set text_box_1 [text $text_frame_1.text -yscrollcommand \
                         "$text_frame_1.text_scrl set"  \
                         -xscrollcommand "$text_frame_1.text_scrl_x set" \
                         -font text_font -wrap none]

    # create the scroll bar for the text box
  
    set text_scroll_bar_1 [scrollbar $text_frame_1.text_scrl -command "$text_box_1 yview"]

    set text_scroll_bar_1a [scrollbar $text_frame_1.text_scrl_x -command "$text_box_1 xview" -orient horizontal]


    # frame for the request display

    set text_frame_2 [frame $win.text_frame_2 -borderwidth 0]  
 
    set text_box_2 [text $text_frame_2.text -yscrollcommand \
                         "$text_frame_2.text_scrl set"  \
                          -xscrollcommand "$text_frame_2.text_scrl_x set" \
                          -font text_font]
  
    # create the scroll bar for the text box
   
    set text_scroll_bar_2 [scrollbar $text_frame_2.text_scrl -command "$text_box_2 yview"]

    set text_scroll_bar_2a [scrollbar $text_frame_2.text_scrl_x -command "$text_box_2 xview" -orient horizontal]

    # frame for the activation display

    set text_frame_3 [frame $win.text_frame_3 -borderwidth 0]  
 
    set text_box_3 [text $text_frame_3.text -yscrollcommand \
                         "$text_frame_3.text_scrl set"  \
                         -xscrollcommand "$text_frame_3.text_scrl_x set" \
                         -font text_font -wrap none]

    # create the scroll bar for the text box
  
    set text_scroll_bar_3 [scrollbar $text_frame_3.text_scrl -command "$text_box_3 yview"]

    set text_scroll_bar_3a [scrollbar $text_frame_3.text_scrl_x -command "$text_box_3 xview" -orient horizontal]



    # frame for the result display

    set text_frame_4 [frame $win.text_frame_4 -borderwidth 0]  
 
    set text_box_4 [text $text_frame_4.text -yscrollcommand \
                         "$text_frame_4.text_scrl set"  \
                         -xscrollcommand "$text_frame_4.text_scrl_x set" \
                         -font text_font -wrap none]

    # create the scroll bar for the text box
  
    set text_scroll_bar_4 [scrollbar $text_frame_4.text_scrl -command "$text_box_4 yview"]

    set text_scroll_bar_4a [scrollbar $text_frame_4.text_scrl_x -command "$text_box_4 xview" -orient horizontal]


    # make chunk selection update the other displays


    global $win.value
    set $win.value ""

    global $win.value2
    set $win.value2 ""

    bind $list_box_1 <<ListboxSelect>> "select_blend_history_time $list_box_1 $list_box_2 $text_box_1 $text_box_2 $text_box_3 $text_box_4 $win.value $win.value2"
    bind $list_box_2 <<ListboxSelect>> "select_blend_history_chunk $list_box_2 $text_box_1 $win.value2"

    button $win.get -text "Get History" -font button_font -command "get_blend_history $list_box_1 $list_box_2 $text_box_1 $text_box_2 $text_box_3 $text_box_4 $win.l0.value $model $win.value"
    button $win.save -text "Save History" -font button_font -command "save_blend_history_data $model"
    button $win.load -text "Load History" -font button_font -command "load_blend_history_data $list_box_1 $list_box_2 $text_box_1 $text_box_2 $text_box_3 $text_box_4 $win.l0.value $model $win.value"

    call_act_r_command record-history $model [list "blending-trace"]

    bind $text_box_1 <Destroy> "call_act_r_command stop-recording-history $model [list blending-trace]
                                global $win.value
                                unset $win.value
                                global $win.value2
                                unset $win.value2" 

   
    pack $list_scroll_bar_1 -side right -fill y 
    pack $list_box_1 -side left -expand 1 -fill both

    pack $list_scroll_bar_2 -side right -fill y 
    pack $list_box_2 -side left -expand 1 -fill both

    pack $text_scroll_bar_1 -side right -fill y
    pack $text_scroll_bar_1a -side bottom -fill x
    pack $text_box_1 -side left -expand 1 -fill both


    pack $text_scroll_bar_2 -side right -fill y
    pack $text_scroll_bar_2a -side bottom -fill x
    pack $text_box_2 -side left -expand 1 -fill both

    pack $text_scroll_bar_3 -side right -fill y
    pack $text_scroll_bar_3a -side bottom -fill x
    pack $text_box_3 -side left -expand 1 -fill both

    pack $text_scroll_bar_4 -side right -fill y
    pack $text_scroll_bar_4a -side bottom -fill x
    pack $text_box_4 -side left -expand 1 -fill both


    place $l0 -relx 0 -y 0 -height 25 -relwidth 1.0

    place $l1 -relx 0.0 -y 25 -height 25 -relwidth .15
    place $list_frame_1 -relx 0.0 -y 50 -relheight .6 -height -50 -relwidth .15


    place $l4 -relx .15 -y 25 -height 25 -relwidth .3
    place $text_frame_2 -relx .15 -y 50 -relheight .3 -height -50 -relwidth .3

    place $l6 -relx .15 -rely .3 -height 25 -relwidth .3
    place $text_frame_4 -relx .15 -rely .3 -y 25 -relheight .3 -height -25 -relwidth .30


    place $l2 -relx .45 -y 25 -height 25 -relwidth .20
    place $list_frame_2 -relx .45 -y 50 -relheight .6 -height -50 -relwidth .20


    place $l3 -relx .65 -y 25 -height 25 -relwidth .35
    place $text_frame_1 -relx .65 -y 50 -relheight .6 -height -50 -relwidth .35


  
    place $l5 -relx .0 -rely .6 -height 25 -relwidth 1.0
    place $text_frame_3 -relx .0 -rely .6 -y 25 -relheight .4 -height -50 -relwidth 1.0

    place $win.get -relx .05 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.save -relx .35 -rely 1.0 -y -25 -height 25 -relwidth .3
    place $win.load -relx .65 -rely 1.0 -y -25 -height 25 -relwidth .3


    # now show the window 

    wm deiconify $win
    focus $win

    return $win
  }
}


proc get_blend_history {lb1 lb2 tb1 tb2 tb3 tb4 label model data_var} {

  global $label

  $lb1 delete 0 end
  $lb2 delete 0 end
  $tb1 delete 1.0 end
  $tb2 delete 1.0 end
  $tb3 delete 1.0 end
  $tb4 delete 1.0 end

  set any [call_act_r_command "history-data-available" $model [list "blending-trace"]]

  if {$any == ""} {
    set $label "No Data available"
  } else {
    set $label "Collecting Data ..."

    set id [call_act_r_command "start-incremental-history-data" $model [list "blending-environment-history" 16000 false]]
 
    if {$id != ""} {
      set result [get_incremental_history $id]
    } else {
      tk_messageBox -icon warning -type ok -title "Get Blending History warning" \
                    -message "Unknown problem occurred trying to get data."
      set $label "Data Error occurred"
      return "" 
    }

    global $data_var
    unset $data_var 

    upvar $data_var data


    foreach i $result {
      set time [expr [lindex $i 0] / 1000.0 ]

      $lb1 insert end $time

      set data($time) [lrange $i 1 end]
    }
      
    set $label "Blending History Data for model $model"
  }
}

proc blend_record_warnings {model s} {
  global blend_history_warnings

  set blend_history_warnings "$blend_history_warnings$s"
  return ""
}

proc load_blend_history_data {lb1 lb2 tb1 tb2 tb3 tb4 label model data_var} {

  global top_dir
  global $label

  set fname [tk_getOpenFile -title "Load Blending History" -initialdir $top_dir] 

  if {$fname != ""} {

    set $label ""

    $lb1 delete 0 end
    $lb2 delete 0 end
    $tb1 delete 1.0 end
    $tb2 delete 1.0 end
    $tb3 delete 1.0 end
    $tb4 delete 1.0 end

    global blend_history_warnings

    set blend_history_warnings ""
            
    set warning_monitor [new_variable_name "warning_monitor"]

    while {[send_cmd "check" $warning_monitor] != "null"} {
      set warning_monitor [new_variable_name "warning_monitor"]
    }

    add_cmd $warning_monitor "blend_record_warnings" "Environment command for capturing warnings during Load Blending History."

    send_cmd "monitor" [list "warning-trace" $warning_monitor]

    set id [call_act_r_command "start-incremental-history-data" $model [list "blending-environment-history" 16000 $fname]]

    send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
    remove_cmd $warning_monitor
     
    if {$id == "" || $id == "null"} {
      tk_messageBox -icon warning -type ok -title "Load Blending History problem" \
                    -message "Get-history-data returned failure result.\n$blend_history_warnings."
      set $label "Failure to load data"
    } else {

      set comment [call_act_r_command "get-incremental-history-data" nil [list $id]]

      set result [get_incremental_history $id]

      global $data_var
      unset $data_var 

      upvar $data_var data


      foreach i $result {
        set time [expr [lindex $i 0] / 1000.0 ]

        $lb1 insert end $time

        set data($time) [lrange $i 1 end]
      }
      
      set $label [json::json2dict [lindex $comment 0]]

    }
  }
}



proc save_blend_history_data {model} {

  global top_dir

  set fname [tk_getSaveFile -title "Save Blending History" -initialdir $top_dir] 

  if {$fname != ""} {

    set any [call_act_r_command "history-data-available" $model [list "blending-trace"]]

    if {$any == "nil" || $any == ""} {
      tk_messageBox -icon warning -type ok -title "Save Blending History warning" \
                    -message "No data available to save with current settings."
    } else {
 
      global blend_history_warnings

      set blend_history_warnings ""
            
      set warning_monitor [new_variable_name "warning_monitor"]

      while {[send_cmd "check" $warning_monitor] != "null"} {
        set warning_monitor [new_variable_name "warning_monitor"]
      }

      add_cmd $warning_monitor "blend_record_warnings" "Environment command for capturing warnings during Save Blending History."

      send_cmd "monitor" [list "warning-trace" $warning_monitor]

      set result [call_act_r_command_with_error_messages "save-history-data" $model [list "blending-environment-history" $fname "Data saved from Blending History window for model $model"]]

      send_cmd "remove-monitor" [list "warning-trace" $warning_monitor]
     
      remove_cmd $warning_monitor
     
      if {[lindex $result 0] == 0} {
        tk_messageBox -icon warning -type ok -title "Save Blending History error" \
                      -message "Save-history-data resulted in error.\n[lindex $result 1]."
      } elseif {[lindex $result 1] == "null"} {
        tk_messageBox -icon warning -type ok -title "Save Blending History problem" \
                      -message "Save-history-data returned failure result.\n$blend_history_warnings."
      }
    }
  }
}

proc select_blend_history_time {lb1 lb2 tb1 tb2 tb3 tb4 data_var data_var2} {

  $lb2 delete 0 end
  $tb1 delete 1.0 end
  $tb2 delete 1.0 end
  $tb3 delete 1.0 end
  $tb4 delete 1.0 end

  set selections [$lb1 curselection]
  if {[llength $selections] != 0} {
    set time [$lb1 get [lindex $selections 0]]
  
    upvar $data_var data

    set vals $data($time)

    $tb2 insert end "+blending>\n[lindex $vals 0]" 
    $tb4 insert end [lindex $vals 1] 
    $tb3 insert end [lindex $vals 3] 

    global $data_var2
    unset $data_var2 

    upvar $data_var2 data2


    foreach i [lindex $vals 2] {
      set name [lindex $i 0]

      $lb2 insert end $name

      set data2($name) [lrange $i 1 end]
    }
  }
}

proc select_blend_history_chunk {lb2 tb1 data_var2} {

  $tb1 delete 1.0 end

  set selections [$lb2 curselection]
  if {[llength $selections] != 0} {
    set name [$lb2 get [lindex $selections 0]]
  
    upvar $data_var2 data

    set vals $data($name)

    $tb1 insert end "Activation: [lindex $vals 0]\n\n"
    $tb1 insert end [lindex $vals 1] 

  }
}




set check_blending [lindex [call_act_r_command "sorted-module-names" "nil"] 0]

if {[lsearch -regexp $check_blending (?i)^blending$] != -1} {

  button [control_panel_name].blending_history -command make_blending_history_viewer -text "Blending History" -font button_font

  pack [control_panel_name].blending_history
}
