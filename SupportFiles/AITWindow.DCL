
ACADE_TOOL : dialog
{
    label = "AIT ACADE TOOL";
    :row{
    : boxed_column {
      label = "Select Folder";
    
    : edit_box { 
      key = "path_editBox"; 
      label = "Path: ";
     }
    
     :row{
            :button
            {
                	label = "Browse";
                	key = "browse_button";
                	width = 10;
                	fixed_width = true;
            }
            :button
            {
                	label = "Enter";
                	key = "path_enter_button";
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
                key = "tool_popupList";
             }
        }
        : boxed_column
        {
            label = "Add Function";
            : button
            {
                	label = "Add Function";
                	key = "add_function_button";
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
            	label = "OK";
            	is_default = true;
            	key = "OK";
            	width = 10;
            	fixed_width = true;
        }
    }
    : errtile {  }
}