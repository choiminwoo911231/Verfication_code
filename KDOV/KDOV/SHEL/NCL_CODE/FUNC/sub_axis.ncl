load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

undef("getprs_value")
function getprs_value(obstype)
 begin

;AMSR2
  if( obstype .eq. "amsr2" ) then
    prs_v = new(5, string)
    prs_v = (/"","","","",""/)
  end if

;CSRGK2A
  if( obstype .eq. "csrgk2a" ) then
    prs_v = new(7, string)
    prs_v = (/"(300)","(400)","(600)","","(1000)","(1000)","(950)"/)
    prs_v = prs_v(::-1)
  end if

;CSRHIMA
  if( obstype .eq. "csrhima" ) then
    prs_v = new(8, string)
    prs_v = (/"(300)","(400)","(600)","","(1000)","","(950)","(900)"/)
    prs_v = prs_v(::-1)
  end if

;CSRMSG
  if( obstype .eq. "csrmsg" ) then
    prs_v = new(5, string)
    prs_v = (/"(400)","(600)","","(1000)","(950)"/)
    prs_v = prs_v(::-1)
  end if

;IASI
  if( obstype .eq. "iasi" ) then
    prs_v = new(31, string)
    prs_v = (/"","","","","(>100)","","","","","","(250)","(200)","","(400)","","(500)","","","","(600)","","","","(800)","","(900)","(850)","(600)","(300)","(400)","(900)"/)
    prs_v = prs_v(::-1)
  end if 

;ATMS
  if( obstype .eq. "atms" ) then
    prs_v = new(15, string)
;    prs_v = (/"(700)","(400)","(250)","(200)","(100)","","","","","","(700)","(600)","(500)","(400)","(300)"/)
    prs_v = (/"(700)","(400)","(250)","(200)","(100)","(50)","(25)","(10)","(5)","(2.5)","(700)","(600)","(500)","(400)","(300)"/)
  end if

;AMSUA
  if( obstype .eq. "amsua" ) then
    prs_v = new(10, string)
;    prs_v = (/"(700)","(400)","(250)","(200)","(100)","","","","",""/)
    prs_v = (/"(700)","(400)","(250)","(200)","(100)","(50)","(25)","(10)","(5)","(2.5)"/)
  end if

;CRIS
  if( obstype .eq. "cris" ) then
    prs_v = new(28, string)
    prs_v = (/"","","","","","","(>100)","","","","","","","","","","(100)","","","","","","","","(300)","","","(700)"/)
    prs_v = prs_v(::-1)
  end if

;MHS
  if( obstype .eq. "mhs" ) then
    prs_v = new(3, string)
    prs_v = (/"(300)","(500)","(700)"/)
    prs_v = prs_v(::-1)
  end if

;MWHS2
  if( obstype .eq. "mwhs2" ) then
    prs_v = new(4, string)
    prs_v = (/"(300)","(400)","(500)","(700)"/)
    prs_v = prs_v(::-1)
  end if




 return(prs_v)

end
