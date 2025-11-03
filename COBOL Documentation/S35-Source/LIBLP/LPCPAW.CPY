      * COPYMEMBER: LIBLP/LPCPAW
      ******************************************************************
      * REV:
      *   BLV  072197 ADD AW-CQ-SEQ-FG
      *   BLV  000817 ADD AW-CQ-START-TIME, TFC PR# 25
      *   BLV  000908 ADD AW-TIME-ZONE-ERROR, TFC PR# 25
      *   BLV  000908 CHANGE AW-START-TIME TO BE SIGNED, TFC PR# 25
      *   BLV  112400 ADD AW-LN-BRNO, TFC PR# 60
      *   BLV  040616 ADD AW-CQ-BEHAVIOR-RISK, REGACC PR# 59
      *    CS  110705 ADD AW-EA-DESC USED FOR THEIR COURT GARN MODEN PR# 3821
      *   BAH  160216 INCREASED RECORD AND FIELDS, PD0003
      *   BAH  171120 REMOVED COMP-3 FROM AW-LN-BRNO
      *    CS  190206 REPLACES LP01AW WHICH MOVED TO QA. ALL EXTRACTIONS WILL
      *               UPDATE IN 1 STEP
      ******************************************************************
       01  AW-WORKERS.
           03  AW-LN-BRNO          PIC 9(4).
           03  AW-COLCTID      PIC X(3).
           03  AW-SETACTCD     PIC X(4).
           03  AW-SETFMNO      PIC 9(4).
           03  AW-INCPROM      PIC 99.
           03  AW-INCCNT       PIC X.
           03  AW-ACCTNO   PIC 9(8).

      $XFD DATE=YYYYMMDD
           03  AW-LTOUCH-DATE      PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  AW-TODAYDUE-DATE    PIC 9(8)     COMP-3.
           03  AW-MORE-FORMS       OCCURS 3 TIMES.
               05  AW-SETFM        PIC 9(4)     COMP-3.
           03  AW-SET-LN-COL       PIC X(1).
           03  AW-CQ-FG            PIC X(1).
           03  AW-CQ-COL-ID        PIC X(3).
           03  AW-REV-DEL          PIC 9(5)     COMP-3.
           03  AW-REV-PDUE-DAYS    PIC 9(5)     COMP-3.
           03  AW-CQ-SEQ-FG        PIC X.
           03  AW-START-TIME       PIC S9(4)    COMP-3.
           03  AW-TIME-ZONE-ERROR  PIC X.
           03  AW-CQ-BEHAVIOR-RISK PIC 9.
           03  AW-EA-DESC          PIC X(4).

           03  FILLER              PIC X(184).
