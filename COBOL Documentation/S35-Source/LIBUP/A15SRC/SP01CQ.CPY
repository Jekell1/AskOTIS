      * COPYMEMBER: LIBSP/SP01CQ
      ******************************************************************
      *               (CQ)
      *    DAILY COLLECTOR QUEUE FILE
      *
      *    CQ-TYPE:    " " = HEADER RECORD. (MN/CQHMAN.C)
      *                "D" = DETAIL RECORD. (MN/CQMAIN.C)
      *
      *    NOTE - I PUT A SIGN ON CQ-TIME & ACTUALLY PUT NEGATIVE
      *    NUMBERS IN THERE FOR THE TIME ZONE FACTOR.  BUT I DON'T
      *    THINK IT MATTERS IF A COMP-3 NUMBER IS NEGATIVE...I MEAN
      *    IN AN INDEX -4 COMES BEFORE -5 (4D, 5D, ETC).  THE POSITIVE
      *    NUMBERS I THINK WOULD COME LATER SINCE THEY'D BE 4F, 5F.
      *    BUT I REALLY WANT THESE NUMBERS TO COME OUT WITH IN NUMERICAL
      *    ORDER WITH THE SIGNS ALLOWED SO THE SEQUENCE IS EAST TO WEST:
      *    EASTERN(-5), THEN  CENTRAL(-6), MOUNTAIN (-7), PACIFIC(-8)
      *
      *    ALSO, NOTE - THE FACTOR STORED HERE HAS ALREADY BEEN ADJUSTED 
      *    BACK AN HOUR FOR DAYLIGHT SAVINGS.  FOR EX., CUSTOMER IS 
      *    EASTERN (-5) BUT ZONE IS CODED DAYLIGHT SAVINGS & IT'S 
      *    JULY SO VALUE OF CQ-TIME IS -4.
      *
      * REV:
      *  040896  BLL  ADD CQ-MISC-KEY-FIELD (FOR POT PAST-DUE)
      *  MJD 990125 ----- UPDATED IN FLEXGEN -----
      *  BLV 000821 PUT A SIGN ON CQ-TIME - IT WAS IMPLIED,
      *             SO DIDN'T CHANGE FILE DEFINITION
      *  BLV 000908 ADD CQ-TIME-ZONE-ERROR, TFC PR# 25
      *  BLV 040615 ADD CQ-REV-BEHAVIOR-RISK, REGACC PR#59
      ******************************************************************
       01  CQ-REC.
           03  CQ1-KEY.
               05  CQ-TYPE              PIC X.
               05  CQ-ACCTNO            PIC 9(8)    COMP-3.
               05  CQ-HEAD-COLLID REDEFINES CQ-ACCTNO
                                        PIC X(5).
           03  CQ2-KEY.
               05  CQ-COLLID            PIC XXX.
               05  CQ-REV-BEHAVIOR-RISK PIC 9.
               05  CQ-SEQ-FG            PIC X.
               05  FILLER               PIC X.
               05  CQ-DATE              PIC 9(7)    COMP-3.
               05  CQ-START-TIME        PIC S9(4)   COMP-3.
               05  CQ-SEQ-FIELDS.
                   07  CQ-REV-DEL       PIC 9(5)    COMP-3.
                   07  CQ-MISC-KEY-FLD  PIC X(3).
                   07  CQ-REV-PDUE-DAYS REDEFINES CQ-MISC-KEY-FLD
                                        PIC 9(5)    COMP-3.
                   07  CQ-REV-BAL       PIC 9(6)    COMP-3.

           03  CQ-RECORD.
               05  CQ-WORK-COLLID       PIC XXX.
               05  CQ-WORKED-FG         PIC X.
               05  CQ-IN-PROCESS        PIC X.
               05  CQ-DATE-QUEUED       PIC 9(7)    COMP-3.
               05  CQ-TIME-ZONE-ERROR   PIC X.
               05  CQ-FILLER            PIC X(13).

           03  CQ-HEADER-REC            REDEFINES CQ-RECORD.
               05  CQ-TOT-REC           PIC 9(5)    COMP-3.
               05  CQ-WORKED-REC        PIC 9(5)    COMP-3.
               05  CQ-TIMED-REC         PIC 9(5)    COMP-3.
               05  CQ-FILLER            PIC X(14).
