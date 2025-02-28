load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

undef("getchn_v")
function getchn_v(obstype,var,stat)
 begin

;AMSR2
  if( obstype .eq. "amsr2" ) then
    v = new(2, float)
	if (stat .eq. "std") then
		v = (/2.4,3.2/) 
	end if                                                   
    if (stat .eq. "mean") then 
		if (var .eq. "OmB") then
	        v = (/3.3,5.1/)                                                     
	    else                                                                    
	        v = (/0,0.4/)                                                       
		end if
    end if                                                                  
  end if
 
;AMSU-A
  if( obstype .eq. "amsua" ) then                                               
    v = new(2, float)                                                          
    if (stat .eq. "std") then                                                   
        v = (/0.3,0.7/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                              
	    v = (/-0.2,0.2/)                                                         
    end if                                                                      
  end if  

;ATMS                                                                      
  if( obstype .eq. "atms" ) then                                               
    v = new(2, float)                                                         
    if (stat .eq. "std") then                                                   
        v = (/0.5,0.9/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        v = (/-0.2,0.2/)                                                        
    end if                                                                      
  end if  

;CrIS                                                                      
  if( obstype .eq. "cris" ) then                                                
    v = new(2, float)                                                         
    if (stat .eq. "std") then                                                   
        v = (/0.08,0.3/)                                                         
    end if                                                                       
    if (stat .eq. "mean") then                                                              
        if (var .eq. "OmB") then                                                
            v = (/0.1,0.5/)                                                     
        else                                                                    
            v = (/-0.04,0.04/)                                                         
        end if                                                                  
    end if  
  end if

;CSRGK2A                                                                       
  if( obstype .eq. "csrgk2a" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/0.5,0.9/)                                                        
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/0.1,0.8/)                                                     
        else                                                                    
            v = (/-0.2,0.2/)                                                  
        end if                                                                  
    end if                                                                      
  end if 

;CSR-Himawari
  if( obstype .eq. "csrhima" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/0.6,1.0/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/-0.3,0.4/)                                                     
        else                                                                    
            v = (/-0.2,0.2/)                                                    
        end if                                                                  
    end if                                                                      
  end if
 
;CSRMSG                                                                        
  if( obstype .eq. "csrmsg" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/0.6,1.0/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/-0.2,0.4/)                                                     
        else                                                                    
            v = (/-0.1,0.1/)                                                    
        end if                                                                  
    end if                                                                      
  end if

;IASI
  if( obstype .eq. "iasi" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/0.3,0.5/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/0.0,0.3/)                                                     
        else                                                                    
            v = (/-0.1,0.1/)                                                    
        end if                                                                  
    end if                                                                      
  end if

;MHS                                                                        
  if( obstype .eq. "mhs" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/1.2,1.6/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/-0.2,0.6/)                                                     
        else                                                                    
            v = (/-0.1,0.1/)                                                    
        end if                                                                  
    end if                                                                      
  end if

;MWHS-2                                                                        
  if( obstype .eq. "mwhs2" ) then                                                
    v = new(2, float)                                                           
    if (stat .eq. "std") then                                                   
        v = (/1.0,1.8/)                                                         
    end if                                                                      
    if (stat .eq. "mean") then                                                  
        if (var .eq. "OmB") then                                                
            v = (/-2.0,-0.4/)                                                     
        else                                                                    
            v = (/-0.1,0.1/)                                                    
        end if                                                                  
    end if                                                                      
  end if


 return(v)

end
