
ACADE_TOOL : dialog
{
    label = "AIT ACADE TOOL";
    : text
    {
        label = "Hello, world.";
    }
    :row
    {
        : boxed_column
        {
            label = "Pick Function";
            : button
            {
                	label = "Dont Click ME";
                	key = "button1";
                	width = 10;
                	fixed_width = true;
            }
            : button
            {
                	label = "Strip MText";
                	key = "STRIP_MTEXT_BUTTON";
                	width = 10;
                	fixed_width = true;
            }
            : button
            {
                	label = "TriCircle";
                	key = "TriCircle";
                	width = 10;
                	fixed_width = true;
            }
            : button
            {
                	label = "BFind";
                	key = "BFind";
                	width = 10;
                	fixed_width = true;
            }
            : button
            {
                	label = "Play troy";
                	key = "troy_button";
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
}