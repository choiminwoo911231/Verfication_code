load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

undef("getsfc_v")
function getsfc_v(obstype,var,idx,stat)
 begin

;SURFACE
  if( obstype .eq. "surface" ) then
    v = new(2, float)
	if (stat .eq. "std") then
		if (idx .eq. "U" .or. idx .eq. "V") then
			v = (/1.5,1.9/) 
		else if idx .eq. "T" then
			v = (/1.8,2.3/)                                                   
        else if idx .eq. "Q" then                                               
            v = (/0.8,1.8/)  
        else if idx .eq. "RH" then                                               
            v = (/9.0,14.0/)  
        else if idx .eq. "PS" then                                               
            v = (/1200,1400/)  
		end if end if end if end if end if
	end if
    if (stat .eq. "mean") then 
		if (var .eq. "OmB") then
	        if (idx .eq. "U" .or. idx .eq. "V") then 
    		    v = (/-0.4,0.2/)                                                     
    	    else if idx .eq. "T" then                                               
				v = (/-1.0,1.0/)
            else if idx .eq. "Q" then                                           
                v = (/-0.4,0.4/) 
            else if idx .eq. "RH" then                                           
                v = (/-10.0,3.0/) 
            else if idx .eq. "PS" then                                           
                v = (/300,600/) 
			end if end if end if end if end if
		end if 
        if (var .eq. "CmB") then                                                
            if (idx .eq. "U" .or. idx .eq. "V") then                            
                v = (/-0.4,0.2/)                                                    
            else if idx .eq. "T" then                                           
                v = (/-1.0,1.0/)                                                
            else if idx .eq. "Q" then                                           
                v = (/-0.4,0.4/)                                                
            else if idx .eq. "RH" then                                          
                v = (/-10.0,3.0/)                                                
            else if idx .eq. "PS" then                                          
                v = (/-20,50/)                                                
            end if end if end if end if end if                                  
        end if 
	end if
  end if


 

;SCATWIND                                                                     
  if( obstype .eq. "scatwind" ) then                                             
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
		v = (/1.1,1.5/)
	else
		v = (/-0.3,0.3/)
    end if 
  end if

 return(v)

end
