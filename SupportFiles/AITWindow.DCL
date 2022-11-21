
ACADE_TOOL : dialog
{
    label = "AIT ACADE TOOL";
    :row{
    : boxed_column {
      label = "Select Folder";
    
    : edit_box { 
      key = "path_EditBox"; 
      label = "Path: ";
     }
    
     :row{
            :button
            {
                	label = "Browse";
                	key = "browse_Button";
                	width = 10;
                	fixed_width = true;
            }
            :button
            {
                	label = "Enter";
                	key = "path_enter_Button";
                	width = 10;
                	fixed_width = true;
            }
     }
    }
    }
    :row
    {
        : boxed_column
        {
            label = "Pick Function";
            : popup_list { 
                key = "tool_PopUpList";
             }
             : button
            {
                	label = "Run";
                	key = "run_function_Button";
                	width = 10;
                	fixed_width = true;
            }
        }
        
        : boxed_column
        {
            label = "Add Function";
            : button
            {
                	label = "Add Function";
                	key = "add_function_Button";
                	width = 10;
                	fixed_width = true;
            }
        }
    }
    :row
    {
        : button
        {
            	label = "Help";
            	key = "Help_Button";
            	width = 10;
            	fixed_width = true;
        }
        : button
        {
            	label = "Exit";
            	is_cancel = true;
            	key = "Exit";
            	width = 10;
            	fixed_width = true;
        }
        : button
        {
            	label = "Settings";
            	
            	key = "open_settings_Button";
            	width = 10;
            	fixed_width = true;
        }
    }
    : errtile {  }
}

settings_Dialog : dialog
{
  label = "AIT ACADE TOOL";
  :row{
    : boxed_column {
      : toggle { 
        label = "Show Start Window";
        key = "settings_show_start_Toggle";
       }
    }
  }
  :row{
    : button
        {
            	label = "Save";
            	key = "settings_save_Button";
            	width = 10;
            	fixed_width = true;
        }
    : button
        {
            	label = "Exit";
            	is_cancel = true;
            	key = "settings_exit_Button";
            	width = 10;
            	fixed_width = true;
        }
  }

}

start_window_Dialog : dialog
{
  label = "AIT ACADE TOOL";
  :row{
    : boxed_column {
      : toggle { 
        label = "Show Start Window";
        key = "settings_show_start_Toggle";
       }
    }
  }
  :row{
    : button
        {
            	label = "Save";
            	key = "settings_save_Button";
            	width = 10;
            	fixed_width = true;
        }
    : button
        {
            	label = "Exit";
            	is_cancel = true;
            	key = "settings_exit_Button";
            	width = 10;
            	fixed_width = true;
        }
  }

}