load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

undef("getprs_v")
function getprs_v(obstype,stat,prs,var)
 begin

;Aircraft
  if( obstype .eq. "aircraft" ) then
    v = new(2, float)
	if (stat .eq. "OmB_std") then
		if (var .eq. "U" .or. var .eq. "V") then
			if prs .eq. 1000 then
				v = (/1.0,6.0/) 
			else if prs .eq. 925 then
				v = (/1.0,5.0/)
            else if prs .eq. 850 then                                           
                v = (/1.0,4.0/)  
            else if prs .eq. 700 then                                           
                v = (/1.5,2.5/)  
            else if prs .eq. 500 then                                           
                v = (/1.6,2.6/)  
            else if prs .eq. 300 then                                           
                v = (/2.0,2.8/)  
            else if prs .eq. 200 then                                           
                v = (/2.2,3.0/)  
            else if prs .eq. 100 then                                           
                v = (/0,100/)  
			end if end if end if end if
			end if end if end if end if
        end if  
        if (var .eq. "T") then                                                  
            if prs .eq. 1000 then                                               
                v = (/0,3.0/)                                                 
            else if prs .eq. 925 then                                           
                v = (/0.4,2.0/)                                                 
            else if prs .eq. 850 then                                           
                v = (/0.4,1.6/)                                                 
            else if prs .eq. 700 then                                           
                v = (/0.4,1.2/)                                                 
            else if prs .eq. 500 then                                           
                v = (/0.4,1.2/)                                                 
            else if prs .eq. 300 then                                           
                v = (/0.5,1.0/)                                                 
            else if prs .eq. 200 then                                           
                v = (/0.7,0.9/)                                                 
            else if prs .eq. 100 then                                           
                v = (/0,50/)                                                  
            end if end if end if end if                                         
            end if end if end if end if                                         
        end if 
	else
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 1000 then                                               
                v = (/-0.6,0.6/)                                                 
            else if prs .eq. 925 then                                           
                v = (/-0.6,0.6/)                                                 
            else if prs .eq. 850 then                                           
                v = (/-0.6,0.6/)                                                 
            else if prs .eq. 700 then                                           
                v = (/-0.4,0.4/)                                                 
            else if prs .eq. 500 then                                           
                v = (/-0.5,0.5/)                                                 
            else if prs .eq. 300 then                                           
                v = (/-0.4,0.4/)                                                 
            else if prs .eq. 200 then                                           
                v = (/-0.2,0.5/)                                                 
            else if prs .eq. 100 then                                           
                v = (/-2.0,2.0/)                                                  
            end if end if end if end if                                         
            end if end if end if end if                                         
        end if                                                                  
        if (var .eq. "T") then                                                  
            if prs .eq. 1000 then                                               
                v = (/-0.1,0.4/)                                                  
            else if prs .eq. 925 then                                           
                v = (/-0.2,0.2/)                                                 
            else if prs .eq. 850 then                                           
                v = (/-0.2,0.2/)                                                 
            else if prs .eq. 700 then                                           
                v = (/-0.2,0.2/)                                                 
            else if prs .eq. 500 then                                           
                v = (/-0.2,0.2/)                                                 
            else if prs .eq. 300 then                                           
                v = (/0.1,0.4/)                                                 
            else if prs .eq. 200 then                                           
                v = (/0.1,0.4/)                                                 
            else if prs .eq. 100 then                                           
                v = (/-0.5,1.5/)                                                   
            end if end if end if end if                                         
            end if end if end if end if                                         
        end if  
    end if                                                                  
  end if
 




;Sonde                                                       
  if( obstype .eq. "sonde" ) then                                            
    v = new(2, float)                                                           
    if (stat .eq. "OmB_std") then                                                   
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 1000 then                                               
                v = (/1.0,15.0/)                                                 
            else if prs .eq. 925 then                                           
                v = (/1.5,4.5/)                                                 
            else if prs .eq. 850 then                                           
                v = (/1.5,4.0/)                                                 
            else if prs .eq. 700 then                                           
                v = (/1.5,4.0/)                                                 
            else if prs .eq. 500 then                                           
                v = (/1.5,5.0/)                                                 
            else if prs .eq. 300 then                                           
                v = (/2.0,12/)                                                 
            else if prs .eq. 200 then                                           
                v = (/1.5,9.0/)                                                 
            else if prs .eq. 100 then                                           
                v = (/1.5,12/)               
            else if prs .eq. 50 then                                           
                v = (/1.5,8/)                                      
            end if end if end if end if                                
            end if end if end if end if end if                                         
        end if                                                                  
        if (var .eq. "T") then                                                  
            if prs .eq. 1000 then                                               
                v = (/0,15.0/)                                                   
            else if prs .eq. 925 then                                           
                v = (/0,6.0/)                                                 
            else if prs .eq. 850 then                                           
                v = (/0,6.0/)                                                 
            else if prs .eq. 700 then                                           
                v = (/0,5.0/)                                                 
            else if prs .eq. 500 then                                           
                v = (/0,4.0/)                                                 
            else if prs .eq. 300 then                                           
                v = (/0,4.0/)                                                 
            else if prs .eq. 200 then                                           
                v = (/0,3.0/)                                                 
            else if prs .eq. 100 then                                           
                v = (/0,4.0/)                
            else if prs .eq. 50 then                                           
                v = (/0,4.0/)                                      
            end if end if end if end if                                         
            end if end if end if end if end if                                         
		end if
        if (var .eq. "Q") then                                                  
            if prs .eq. 1000 then                                               
                v = (/0,10.0/)                                                  
            else if prs .eq. 925 then                                           
                v = (/0,5.0/)                                                   
            else if prs .eq. 850 then                                           
                v = (/0,5.0/)                                                   
            else if prs .eq. 700 then                                           
                v = (/0,6.0/)                                                   
            else if prs .eq. 500 then                                           
                v = (/0,3.0/)                                                   
            else if prs .eq. 300 then                                           
                v = (/0,0.6/)                                                   
            end if end if end if end if                                         
            end if end if 
        end if                                                                  
    else                                                                        
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 1000 then                                               
                v = (/-1.5,1.5/)                                                
            else if prs .eq. 925 then                                           
                v = (/-0.9,0.9/)                                                
            else if prs .eq. 850 then                                           
                v = (/-0.8,0.8/)                                                
            else if prs .eq. 700 then                                           
                v = (/-0.8,0.8/)                                                
            else if prs .eq. 500 then                                           
                v = (/-0.8,0.8/)                                                
            else if prs .eq. 300 then                                           
                v = (/-1.2,1.2/)                                                
            else if prs .eq. 200 then                                           
                v = (/-1.0,1.0/)                                                
            else if prs .eq. 100 then                                           
                v = (/-1.2,1.2/)            
            else if prs .eq. 50 then                                           
                v = (/-1.0,1.0/)                                     
            end if end if end if end if                                         
            end if end if end if end if end if
        end if                                                                  
        if (var .eq. "T") then                                                  
            if prs .eq. 1000 then                                               
                v = (/-1.5,1.5/)                                                
            else if prs .eq. 925 then                                           
                v = (/-1.2,1.2/)                                                
            else if prs .eq. 850 then                                           
                v = (/-1.2,1.2/)                                                
            else if prs .eq. 700 then                                           
                v = (/-1.2,1.2/)                                                
            else if prs .eq. 500 then                                           
                v = (/-0.8,0.8/)                                                
            else if prs .eq. 300 then                                           
                v = (/-0.8,0.8/)                                                 
            else if prs .eq. 200 then                                           
                v = (/-0.8,0.8/)                                                 
            else if prs .eq. 100 then                                           
                v = (/-0.9,0.9/)              
            else if prs .eq. 50 then                                           
                v = (/-0.9,0.9/)                                     
            end if end if end if end if                                         
            end if end if end if end if end if
        end if                                                                  
        if (var .eq. "Q") then                                                  
            if prs .eq. 1000 then                                               
                v = (/-1.2,1.2/)                                                
            else if prs .eq. 925 then                                           
                v = (/-0.6,0.6/)                                                
            else if prs .eq. 850 then                                           
                v = (/-0.8,0.8/)                                                
            else if prs .eq. 700 then                                           
                v = (/-0.6,0.6/)                                                
            else if prs .eq. 500 then                                           
                v = (/-0.4,0.4/)                                                
            else if prs .eq. 300 then                                           
                v = (/-0.2,0.2/)                                                 
            end if end if end if end if                                         
            end if end if 
        end if 
    end if                                                                      
  end if 




;AMV                                         
  if( obstype .eq. "amv" ) then                                               
    v = new(2, float)                                                           
    if (stat .eq. "OmB_std") then                                               
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 1000 then                                               
                v = (/1.0,1.8/)                                                
            else if prs .eq. 925 then                                           
                v = (/1.0,2.0/)                                                 
            else if prs .eq. 850 then                                           
                v = (/1.6,2.6/)                                                 
            else if prs .eq. 700 then                                           
                v = (/2.6,3.6/)                                                 
            else if prs .eq. 500 then                                           
                v = (/2.6,3.4/)                                                 
            else if prs .eq. 300 then                                           
                v = (/3.0,3.6/)                                                  
            else if prs .eq. 200 then                                           
                v = (/2.8,3.6/)                                                 
            end if end if end if end if                                         
            end if end if end if
        end if         
	else
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 1000 then                                               
                v = (/-0.1,0.4/)                                                
            else if prs .eq. 925 then                                           
                v = (/0,0.5/)                                                 
            else if prs .eq. 850 then                                           
                v = (/-0.5,0.1/)                                                 
            else if prs .eq. 700 then                                           
                v = (/-0.8,0/)                                                 
            else if prs .eq. 500 then                                           
                v = (/-0.4,0.2/)                                                 
            else if prs .eq. 300 then                                           
                v = (/-0.4,0.2/)                                                  
            else if prs .eq. 200 then                                           
                v = (/-0.6,0.2/)                                                 
            end if end if end if end if                                         
            end if end if end if
        end if  
	end if
  end if                




;GPSRO
  if( obstype .eq. "gpsro" ) then                                                 
    v = new(2, float)                                                           
    if (stat .eq. "OmB_std") then                                               
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 300 then                                               
                v = (/1.0,1.8/)                                                 
            else if prs .eq. 200 then                                           
                v = (/0.5,1.2/)                                                 
            else if prs .eq. 100 then                                           
                v = (/0.3,0.6/)                                                 
            else if prs .eq. 50 then                                           
                v = (/0.1,0.3/)  
            else if prs .eq. 25 then                                           
                v = (/0.05,0.15/)
            else if prs .eq. 10 then                                           
                v = (/0.02,0.06/)    
            end if end if end if end if                                         
            end if end if                                                 
        end if                                                                  
    else                                                                        
        if (var .eq. "U" .or. var .eq. "V") then                                
            if prs .eq. 300 then                                               
                v = (/-0.3,0.1/)                                                
            else if prs .eq. 200 then                                           
                v = (/-0.2,0.1/)                                                   
            else if prs .eq. 100 then                                           
                v = (/-0.1,0.1/)                                                
            else if prs .eq. 50 then                                           
                v = (/-0.05,0.05/)                                                  
            else if prs .eq. 25 then                                           
                v = (/-0.05,0.05/)                                                
            else if prs .eq. 10 then                                           
                v = (/-0.05,0.05/)                                                
            end if end if end if end if                                         
            end if end if                                                 
        end if                                                                  
    end if                                                                      
  end if 



 return(v)

end
