      * COPYMEMBER: LIBLP/LPFDTRI.CPY
      ******************************************************************
      *
      * DESCRIPTION:  TRANSACTION FILE [INSURANCE CLAIMS]
      *
      * FILE INFO  :  INSURANCE CLAIMS, THESE ARE MEMO PURPOSE ONLY
      *               LP/TRIFILE
      *
      * TRI1-KEY = TI-BRNO, TI-KEY-DATE, TI-SEQ
      * TRI2-KEY = TI-KEY-DATE, TI-BRNO, TI-SEQ
      *
      * ***NOTE*** :  THESE ARE MEMO ONLY, FOR REPORTING PURPOSES ONLY
      *
      *=================================================================
      * REV:
      * BAH 040204 NEW FOR WORLD INSURANCE CLAIMS UPDATE
      * BLV 040621 ADD TI-CLAIM-NUMBER
      * BLF 050111 ADD TI-INSCOMP, WORLD PR# 366
      * BLM 090318 ADD TI-DAYS-PAID TI-PMT-TYPE, TI-FROM-DATE,
      *            TI-TO-DATE, WORLD PR# 631
      *  CS 160915 CHANGED TI-POSTDATE,TI-FROM-DATE,TI-TODATE FROM
      *            9(6) TO 9(8)
      *            SAVED VERSION LIBLP/LPFDTRI.160921
      *
      * KEC 2017.0120 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE FOLLOWING SINCE NOT USED ANYWHERE:
      *            05  TI-PRODUCER-ALP REDEFINES TI-PRODUCER.
      *                07  TI-PRODUCER1        PIC X.
      *                07  TI-NEW-BRNO         PIC 9(5).
      * BAH 180125 REDUCED FILLER AND FILE SIZE PER KM
      * BAH 20200324 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * BAH 20200508 REMOVED TI-DRAFT-ALP AND MOVED TI-DRAFT FROM 07 TO 05
      *              REMOVED TI-AMT REDEFINES TI-AMOUNT
      * BAH 20210107 RENAMED TI-DAY-DATE TO TI-KEY-DATE
      * BAH 20210426 REMOVED FUTURE FIELDS
      ******************************************************************
       78  TRI-SIZE                    VALUE 395.
       01  TRI-REC.
           03  TRI1-KEY.
               05  TI-BRNO             PIC 9(4).
      $XFD DATE=YYYYMMDD
               05  TI-KEY-DATE         PIC 9(8). 
               05  TI-SEQ              PIC 9(5).

      $XFD DATE=YYYYMMDD
           03  TI-LTOUCH-DATE          PIC 9(8) COMP-3.

           03  TI-CLAIMS-REC.
               05  TI-WAIS                 PIC X(4).
               05  TI-FILLER               PIC X(4).
               05  TI-TYPE                 PIC 9(3).
                   88 TI-TYPE-CL           VALUE 510.
                   88 TI-TYPE-AH           VALUE 511.
                   88 TI-TYPE-PP           VALUE 527.
                   88 TI-TYPE-AUTO         VALUE 528.
                   88 TI-TYPE-BK-NONF      VALUE 536.
                   88 TI-TYPE-NONF         VALUE 538.
                   88 TI-TYPE-UNEMP        VALUE 454.
                   88 TI-TYPE-AD-D         VALUE 459.
                   88 TI-TYPE-L-PROT       VALUE 470.
                   88 TI-TYPE-ROADG        VALUE 463.
                   88 TI-TYPE-VALID        VALUE 454 459 463 470 510
                                                 511 527 528 536 538.
               05  TI-AMOUNT               PIC S9(8)V99.
               05  TI-CLAIM-NUMBER         PIC X(9).
               05  TI-FILLER2              PIC X(13).
               05  TI-LNAME                PIC X(15).
               05  TI-FNAME                PIC X(10).
      $XFD DATE=YYYYMMDD
               05  TI-DATE                 PIC 9(8).
               05  TI-DRAFT                PIC 9(8).
               05  TI-FILLER3              PIC X(40).
               05  TI-PRODUCER             PIC 9(6).

           03  TI-USERID               PIC X(8).
           03  TI-POSTTIME             PIC 9(6)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  TI-POSTDATE             PIC 9(8)     COMP-3.
           03  TI-INSCOMP              PIC 9(2).                   
           03  TI-DAYS-PAID            PIC 9(6)     COMP-3.
           03  TI-PMT-TYPE             PIC X(10).
      $XFD DATE=YYYYMMDD
           03  TI-FROM-DATE            PIC 9(8)     COMP-3.
      $XFD DATE=YYYYMMDD
           03  TI-TO-DATE              PIC 9(8)     COMP-3.
           03  FILLER                  PIC X(200).
