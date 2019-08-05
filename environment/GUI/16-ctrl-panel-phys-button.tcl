button .control_panel.phys_button -command {openPhys} -text "Physiology" -font button_font

openPhys {}
{
	if {[currently_selected_model] == "nil"} 
	{

		tk_messageBox -icon info -type ok -title "Physiology" -message "Inspector tools require a current model."
	} 
	else 
	{

		set win ".phys_[currently_selected_model]"
		
		if {[winfo exists $win] == 1}
		{
			wm deiconify $win
			raise $win
		}
		else
		{
			toplevel $win

			wm withdraw $win

			record_new_window $win "Physiology"

			wm geometry $win [get_configuration .phys $win]

			set f [frame $win.frame -borderwidth 0]  

			set t [text $f.text -font text_font -yscrollcommand "$f.scrl set" -state disabled]
			  
			set s [scrollbar $f.scrl -command "$t yview"]

			send_environment_cmd \
			"create text-output-handler $t $t \
				(lambda (x) (declare (ignore x)) (print-phys)) (post) [send_model_name]"

			bind $t <Destroy> {
			remove_handler %W
			}

			# Make the window useable for copy operations on Windows

			bind $t <1> {focus %W}

			pack $s -side right -fill y 
			pack $t -side left -expand 1 -fill both

			place $f -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

			# now show the window 

			wm deiconify $win
		}
	}
}

pack .control_panel.phys_button