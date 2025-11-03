      * COPYMEMBER: LIBLP/LP01FT
      ******************************************************************
      *     FORM DESCRIPTOR TRAILER FILE   RSZ 1000
      *
      *     KEY 1 - FMNO                                
      *
      *     FMNO:
      *          FD-REC CONTAINS 1 FORM DESCRIPTOR
      *                 UP TO 999 RECORDS (FMNO 1-999)              
      *
      * REV:                                            
      * BAH 130325 NEW, REGACC PR# 335
      ******************************************************************
       01  FT-REC.
           03  FT1-KEY.
               05  FT-FMNO             PIC 9(3).

           03  FT-FORM-RIGHT-TO-CURE   PIC X.
           03  FT-TEST-WHICH-STATE     PIC X.
           03  FT-STATE-TABLE.
               05  FT-STATE            PIC XX OCCURS 50.
               
           03  FILLER                  PIC X(895).

