      * COPYMEMBER: LIBUP/UPALPCL
      ******************************************************************
      *
      * DESCRIPTION: ASCII LOAN PAYMENT FILE CLEAR ROUTINE
      *
      ******************************************************************
       CLEAR-ALP-FILE SECTION.
           MOVE ALL X"09" TO ALP-REC.

           MOVE 0 TO ALP-TRDATE ALP-PAYDATE
                     ALP-TRAMT ALP-APOTH ALP-APOT2 ALP-APINT ALP-APLC
                     ALP-APCUR ALP-OTHBAL ALP-OT2BAL ALP-INTBAL
                     ALP-LCBAL ALP-CURBAL ALP-INTDUE ALP-DLPROC ALP-DLNO
                     ALP-EARNED(1) ALP-EARNED(2) ALP-EARNED(3)
                     ALP-INTPAID ALP-SSNO ALP-PDTH-DATE ALP-LCPDTH-DATE
                     ALP-APINTOWE ALP-REVSEQNO
                     ALP-NEWSTAT-RATE ALP-TILLNO ALP-ESCROW-PREPAID
                     ALP-POSTDATE ALP-POSTTIME
                     ALP-DLRVCHGS-ADJ(1) ALP-DLRVCHGS-ADJ(2)
                     ALP-DLRVCHGS-ADJ(3)
                     ALP-DLPARVHELD-ADJ ALP-DLPARVPAID-ADJ
                     ALP-POSTING-BR ALP-NOLCHG
                     ALP-EXTRA-PRIN-LFP ALP-REMAIN-INT-SINCE-LFP
                     ALP-REMAIN-PRIN-SINCE-LFP ALP-APPLIED-TO-ESCROW 
                     ALP-ADDON-INTPDTH-DATE
      *NEW LP FIELDS
                     ALP-INTPDTH-DATE ALP-PRIOR-POOLID ALP-LT-SEQNO
                     ALP-NEW1ST-PYDATE ALP-DEF-DUEDATE
                     ALP-ADDON-ORIG-REGPY ALP-OLD1ST-PYDATE
                     ALP-DEF-TERM ALP-LCPARTIALS
                     ALP-LCPAID ALP-PREPAY-PENALTY  
                     ALP-PRORATED-INS-REB ALP-ADDON-LAST-PYAMT 
                     ALP-FROMCL ALP-TOCL ALP-BRNO ALP-LTOUCH-DATE.

           MOVE SPACES TO ALP-USERID ALP-TRCD ALP-REFNO ALP-IBPC           
                          ALP-MANUAL-RB ALP-INSTYPES ALP-REV            
                          ALP-REVTRCD ALP-STATUSFG ALP-COLLECTOR       
                          ALP-ALT-PREPAY-FG.

