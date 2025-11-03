       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPMAN3.
       AUTHOR.           CS.
       DATE-WRITTEN.    10-19-2015.
      ******************************************************************
      *
      *  IDENTIFICATION:  MN/LPMAN3.C
      *
      *  DESCRIPTION   :  LOAN PAYMENT FILE MAINTENANCE - PAGE 3
      *
      *-----------------------------------------------------------------
      * REV:
      *  CS 151019 CREATED NEW PAGE 3 
      *  CS 151113 USE EXT-FILPATH-BRANCH TO GET BRNO, SINCE ITS NOW PART
      *            OF LP1-KEY, SEQNO IS NOW 5
      *  CS 151120 A30 NEW FIELDS:  LP-NEW1ST-PAYDATE,LP-DEF-DUEDATE
      *            LP-ADDON-ORIG-REGPY,LP-LCPARTIALS,LP-OLD1ST-PAYDATE
      *            LP-DEF-TERM,LP-LCPAID,LP-PREPAY-PENALTY,LP-PRORATED-INS-REB
      *            LP-PRIOR-POOLID, LP-INSTYPES, LP-LT-SEQNO, LP-RT-SEQNO
      *            LP-INT-PDTH, LP-ADDON-INTPDTH
      *  CS 151216 ADD LP-LTOUCH-DATE
      *  CS 160201 ADD LP-FROMCL, LP-TO CL, WAS A REDEFINE OF LP-REFNO
      *  CS 170426 ACTIVATE 8 DIGIT DATES
      * BAH 190103 GLOBAL LPFILE
      * BAH 190307 REMOVED LP1-KEY9, USE A WORKING STORAGE SCAN-LP1-KEY9
      * BAH 20190627 REMOVED LP-RT-SEQ 13.
      * KEC 200214 CHANGED THE FORM ENTRY FOR LP-SEQNO FROM 4 TO 5 BYTES
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LPMAN3.CBL S35 02/21/24-13:01:02 >".

       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".

      * ORIGINAL PAYMENT RECORD LPOR
      * CHANGED  PAYMENT RECORD LPOR
      
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPOR".
       COPY "LIBLP/LP01LP.CPY" REPLACING LEADING "LP" BY "LPCH".

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(12).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(11).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(10)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(7).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(11).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(10)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(7).99-.


      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION                                    VALUE SPACES.
           03  CHKEY                   PIC X(04).
           03  CHNO                    PIC 9(02).
           03  CHNO-X                  REDEFINES CHNO
                                       PIC X(02).

       01  FORM-NUM1                   REDEFINES ACTION.
           03  FORM-KEY1               PIC X(04).
           03  FORM-P1                 PIC X(01).
           03  FORM-SEL1               PIC 9(01).

      *-----------------------------------------------------------------
       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.
       01  SCAN-LP1-KEY9   PIC 9(13).
       01  SCAN-LP1-KEY REDEFINES SCAN-LP1-KEY9.
           03  SCAN-LP-ACCTNO  PIC 9(8).
           03  SCAN-LP-SEQNO   PIC 9(5).

       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.

      * 'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      
       01  PAGE-CHANGE-FLAG            PIC X(01)     VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".

       01  LPMAIN-LINK                 VALUE SPACES.
           03  FORM-NAME               PIC X(20).
           03  SAVEKEY.                               
               05  SAVEKEY-BRNO        PIC 9(4).
               05  SAVEKEY-ACCTNO      PIC 9(8).
               05  SAVEKEY-SEQNO       PIC 9(5).
           03  NEW-REC                 PIC X(01).
           03  ERRCD                   PIC X(01).
           03  FORMSEL                 PIC 9(02).
           03  LPMAIN-WINDOW-HANDLE    PIC X(10).
       
       01  DISPLAY-BUF                 VALUE SPACES.
      * DC NEW 1ST PAYDATE 
           03  F01                     PIC   99/99/99  BLANK ZERO.
      * DEFERMENT DUE DATE
           03  F02                     PIC   99/99/99  BLANK ZERO.
      * ADDON ORIG REG PAYAMT
           03  F03                     PIC ZZZZZZ9.99- BLANK ZERO.
      * LAST CHG PMT DUE AMT
           03  F04                     PIC ZZZZZZ9.99- BLANK ZERO.
      * DC OLD 1ST PAYDATE
           03  F05                     PIC  99/99/99  BLANK ZERO.
      * DEFERMENT TERM
           03  F06                     PIC ZZ9   BLANK ZERO.
      * LATE CHARGE PAID
           03  F07                     PIC ZZZZZZ9.99- BLANK ZERO.
      * PAYOFF PREPAY PENALTY
           03  F08                     PIC ZZZZZZ9.99- BLANK ZERO.
      * PRORATED INS REBATE
           03  F09                     PIC ZZZZZZ9.99- BLANK ZERO.
      * PRIOR DLR POOL ID
           03  F10                     PIC ZZZZ9 BLANK ZERO.
      *ADDON INSURANCE TYPES
           03  F11                     PIC X.                         
      *INSURANCE TRAILER SEQ               
           03  F12                     PIC ZZ9 BLANK ZERO.             
      *N/A
           03  F13                     PIC ZZZZ9   BLANK ZERO.
      *INTEREST PAID THRU DATE
           03  F14                     PIC 99/99/99  BLANK ZERO.
      *ADDON LAST PAYAMT      
           03  F15                     PIC ZZZZZZ9.99- BLANK ZERO.
      *ADDON INT PDTH         
           03  F16                     PIC 99/99/99    BLANK ZERO.
      *TRANSFER FROM CLASS
           03  F17                     PIC ZZ          BLANK ZERO.
      *TRANSFER TO CLASS               
           03  F18                     PIC ZZ          BLANK ZERO.
      *TESTING LP-BRNO
           03  DBRNO                   PIC 9999        BLANK ZERO.
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(24) VALUE "110 120 130 140 150 160 ".            
           03  FILLER PIC X(14) VALUE "170 180 DBRNO.".

      ******************************************************************
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      ******************************************************************
       01  ELE                         PIC 9(02)     VALUE ZEROES.

       77  ONE                         PIC S9(1)     VALUE ZEROES.
       78  MAX                                       VALUE 18.

       01  SPEC-TABLE.
      *DC NEW 1ST PAYDATE
           03  FILLER PIC X(8) VALUE "ERNM060N".
      *DEFERMENT DUE DATE
           03  FILLER PIC X(8) VALUE "ERNM060N".
      *ADDON ORIG REG PAYAMT
           03  FILLER PIC X(8) VALUE "ENNS072N".
      * LAST CHG PMT DUE AMT
           03  FILLER PIC X(8) VALUE "ENNS072N".
      *DC OLD 1ST PAYDATE
           03  FILLER PIC X(8) VALUE "ERNM060N".
      *DEFERMENT TERM
           03  FILLER PIC X(8) VALUE "ENNN030N".
      *LATE CHARGE PAID
           03  FILLER PIC X(8) VALUE "ENNS072N".
      *PAYOFF PREPAYMENT PENALTY
           03  FILLER PIC X(8) VALUE "ENNS072N".
      *PRORATED INS REBATE
           03  FILLER PIC X(8) VALUE "ENNS072N".
      *PRIOR DEALER POOL
           03  FILLER PIC X(8) VALUE "ENNN050N".
      *ADDON INS TYPES
           03  FILLER PIC X(8) VALUE "E  A010N".
      *ADDON INS TRAILER SEQ
           03  FILLER PIC X(8) VALUE "ENNN030N".
      *N/A
           03  FILLER PIC X(8) VALUE "ENNN050N".
      *INTEREST PAID THRU
           03  FILLER PIC X(8) VALUE "ERNM060N".
      *ADDON LAST PAYAMT
           03  FILLER PIC X(8) VALUE "ENNS072N".
      *ADDON INTEREST PAID THRU
           03  FILLER PIC X(8) VALUE "ERNM060N".
      *TRANSFER FROM CLASS
           03  FILLER PIC X(8) VALUE "EN N020N".
      *TRANSFER TO CLASS
           03  FILLER PIC X(8) VALUE "EN N020N".


       01  SPEC-TABLE2                 REDEFINES SPEC-TABLE.
           03  SPEC-REC                OCCURS MAX TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LPMAN3_DEF.CPY".
       COPY "LIBMN/LPMAN3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       
      *    L I N K A G E - S E C T I O N
       
       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA          PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LPMAN3_SCN.CPY".


      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE FORM-LINK-AREA TO LPMAIN-LINK.

           MOVE "LPMAN3"      TO VDU-FORM-NAME.

           MOVE SPACES TO VDU-WIMENU-SELECTION
                          VDU-PGMENU-SELECTION.


       MAIN-PROGRAM-LOOP.
           PERFORM FORM-RESET.

           PERFORM ENTER-FILE-KEY.
           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO MAIN-PROGRAM-EXIT.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-PROGRAM-LOOP.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "N" TO NEW-REC
              MOVE "P" TO FORM-P1
              MOVE 1   TO FORM-SEL1
              MOVE SPACES TO SAVEKEY.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE LP1-KEY   TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO MAIN-PROGRAM-EXIT.

           GO TO MAIN-PROGRAM-LOOP.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       ENTER-FILE-KEY SECTION.
      ******************************************************************

           MOVE 0 TO LP-ACCTNO
                     LP-SEQNO
                     FIRST-SCAN.
           MOVE " " TO IO-TYPE.

           IF SAVEKEY = " "
              MOVE 0    TO NEW-FLAG
              MOVE "N"  TO PAGE-CHANGE-FLAG
              GO TO ENTER-FILE-KEY-ACCTNO.

      * KEY WAS PASSED FROM ANOTHER PAGE 

           MOVE "Y"             TO PAGE-CHANGE-FLAG.
           MOVE SAVEKEY         TO LP1-KEY.
           MOVE " "             TO SAVEKEY.

           MOVE "SN N080LPNO."  TO VDU.
           MOVE LP-ACCTNO       TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N050LPSEQ." TO VDU.
           MOVE LP-SEQNO        TO VDUNUM0.
           PERFORM SCREENIO.

           IF NEW-REC = "Y"
              PERFORM SETUP-NEW-RECORD.

           GO TO ENTER-FILE-KEY-READ-FILE.

       ENTER-FILE-KEY-ACCTNO.
           MOVE "EN N080LPNO." TO VDU.
           PERFORM SCREENIO.

           IF F6-KEY
              PERFORM ENTER-SCAN-KEY
              GO TO ENTER-FILE-KEY-EXIT.

           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-FILE-KEY-EXIT.

           IF NOT ENTER-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.

           MOVE VDUNUM0 TO LP-ACCTNO.

           IF LP-ACCTNO = 0
              GO TO ENTER-FILE-KEY-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-FILE-KEY-SEQNO.
           MOVE "EN N050LPSEQ." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.
           IF F1-KEY
              GO TO ENTER-FILE-KEY-ACCTNO.
           IF F6-KEY
              PERFORM ENTER-SCAN-KEY
              GO TO ENTER-FILE-KEY-EXIT.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO ENTER-FILE-KEY-EXIT.
           IF NOT ENTER-KEY
              GO TO ENTER-FILE-KEY-SEQNO.

           MOVE VDUNUM0 TO LP-SEQNO.
           IF LP-SEQNO = 0
              GO TO ENTER-FILE-KEY-SEQNO.

       
       ENTER-FILE-KEY-READ-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-BAD
              MOVE "CLR"               TO IO-TYPE
              MOVE "** NOT ON FILE **" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FILE-KEY-EXIT.

           MOVE LP-REC TO LPOR-REC.

           PERFORM DISPLAY-FIELDS.

       ENTER-FILE-KEY-EXIT.
            EXIT.

      ******************************************************************
       ENTER-SCAN-KEY SECTION.
      ******************************************************************
           MOVE " ENTER PAYMENT #, OR F6 TO SCAN" TO TMMSG-MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE 0             TO LP-ACCTNO LP-SEQNO.

           PERFORM START-LP1-FILE.

       
       ENTER-SCAN-KEY-ENTRY.

           MOVE "EN N130SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-KEY-END.
           IF F6-KEY
              GO TO ENTER-SCAN-KEY-READ-FILE.
           IF NOT ENTER-KEY
              GO TO ENTER-SCAN-KEY-ENTRY.

           IF VDUNUM0 = 0
              IF FIRST-SCAN NOT = 0
                 MOVE LP-REC TO LPOR-REC
                 GO TO ENTER-SCAN-KEY-END
              ELSE
                 GO TO ENTER-SCAN-KEY-ENTRY.

      *    MOVE VDUNUM0 TO LP1-KEY9.

      * SCAN IS STILL JUST ACCTNO/SEQNO, NO BRANCH YET
           MOVE VDUNUM0 TO SCAN-LP1-KEY9.

           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE SCAN-LP-ACCTNO TO LP-ACCTNO.
           MOVE SCAN-LP-SEQNO  TO LP-SEQNO.


           PERFORM START-LP1-FILE.

      *-----------------------------------------------------------------
       ENTER-SCAN-KEY-READ-FILE.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCAN-KEY-END.

      *    UNLOCK LP-FILE.

           MOVE "SN N080LPNO." TO VDU.
           MOVE LP-ACCTNO      TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N050LPSEQ." TO VDU.
           MOVE LP-SEQNO        TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.

           GO TO ENTER-SCAN-KEY-ENTRY.

       ENTER-SCAN-KEY-END.
           MOVE 0   TO FIRST-SCAN.
           PERFORM CLOSE-LP1-FILE.

           MOVE SPACES TO TMMSG-MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCAN-KEY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       
       CREATE-ROUTINE.

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE
                        ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( NOT ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
      
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P"    ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 < 1 ) OR
                 ( FORM-SEL1 > 3 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO = ZEROES      ) GO TO MODIFY-ROUTINE.
           IF ( CHNO > MAX         ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ( CHKEY NOT    = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG  = ZEROES ) AND
              ( CHFG(ELE) = "N"    ) AND
              ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD   )
              GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

           IF ( F1-KEY )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

           IF ( DOWN-KEY )
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

           IF ( NOT ENTER-KEY )
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
       
       F-01.
           MOVE VDU-DATE-YYYYMMDD TO LP-NEW1ST-PYDATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDU-DATE-YYYYMMDD TO LP-DEF-DUEDATE.    
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO LP-ADDON-ORIG-REGPY.     
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM2 TO LP-LCPARTIALS.            
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-05.
           MOVE VDU-DATE-YYYYMMDD TO LP-OLD1ST-PYDATE
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM0 TO LP-DEF-TERM.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO LP-LCPAID.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO LP-PREPAY-PENALTY.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO LP-PRORATED-INS-REB.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM0 TO LP-PRIOR-POOLID.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           MOVE VDUBUF TO LP-INSTYPES.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           IF LP-TRCD = "ET" OR "RT" OR "LP" OR "NP" OR "ST"
              MOVE VDUNUM0 TO LP-LT-SEQNO.
           GO TO CREATE-ROUTINE.
       F-13.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDU-DATE-YYYYMMDD TO LP-INTPDTH-DATE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO LP-ADDON-LAST-PYAMT. 
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDU-DATE-YYYYMMDD TO LP-ADDON-INTPDTH-DATE.        
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO LP-FROMCL.                 
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM0 TO LP-TOCL.                 
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
       
           MOVE ZEROES TO FORM-SEL1.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE LP-NEW1ST-PYDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY               TO F01.

           MOVE LP-DEF-DUEDATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F02.

           MOVE LP-ADDON-ORIG-REGPY        TO F03.
           MOVE LP-LCPARTIALS              TO F04.

           MOVE LP-OLD1ST-PYDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F05.
           MOVE LP-DEF-TERM                TO F06.
           MOVE LP-LCPAID                  TO F07.
           MOVE LP-PREPAY-PENALTY          TO F08.
           MOVE LP-PRORATED-INS-REB        TO F09.
           MOVE LP-PRIOR-POOLID            TO F10.
           MOVE LP-INSTYPES                TO F11.
           MOVE LP-LT-SEQNO                TO F12.
           MOVE 0                          TO F13.
           MOVE LP-INTPDTH-DATE            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F14.
           MOVE LP-ADDON-LAST-PYAMT        TO F15.
           MOVE LP-ADDON-INTPDTH-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO F16.
           MOVE LP-FROMCL                  TO F17.
           MOVE LP-TOCL                    TO F18.
           MOVE LP-BRNO                    TO DBRNO.

           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       SETUP-NEW-RECORD SECTION.
      ******************************************************************

           MOVE ZEROES TO ELE.
           MOVE 1      TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

       SETUP-NEW-RECORD-EXIT.
           EXIT.


      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-LP1-FILE.

       WRITE-MODULE-REWRITE-LP.

           MOVE LP-REC TO LPCH-REC.

           IF LPOR-REC = LPCH-REC
              GO TO WRITE-MODULE-REWRITE-END.

           PERFORM READ-LP1-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO WRITE-MODULE-END.

           IF LP-REC NOT = LPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO WRITE-MODULE-END.

           PERFORM LOG-MAINTENANCE-CHGS-LP.

           MOVE LPCH-REC TO LP-REC.

           PERFORM REWRITE-LP1-FILE.

       WRITE-MODULE-REWRITE-END.
           GO TO WRITE-MODULE-END.

       WRITE-MODULE-END.
           PERFORM CLOSE-LP1-FILE.

       WRITE-MODULE-EXIT.
           EXIT.

      ******************************************************
       LOG-MAINTENANCE-CHGS-LP SECTION.
      ******************************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LP-BRNO   TO WORK-ERRLOG-BR.
           MOVE LP-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO WORK-ERRLOG-TO-X.

           IF LPOR-NEW1ST-PYDATE NOT = LPCH-NEW1ST-PYDATE
             MOVE "01 "                  TO WORK-ERRLOG-FIELD
             MOVE "DC 1STPYDATE"         TO WORK-ERRLOG-DESC
             MOVE LPOR-NEW1ST-PYDATE     TO DATE-YYYYMMDD       
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-NEW1ST-PYDATE     TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DEF-DUEDATE   NOT = LPCH-DEF-DUEDATE   
             MOVE "02 "                  TO WORK-ERRLOG-FIELD
             MOVE "DF DUE DATE"   TO WORK-ERRLOG-DESC
             MOVE LPOR-DEF-DUEDATE       TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-ORG-N0
             MOVE LPOR-DEF-DUEDATE       TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-ADDON-ORIG-REGPY NOT =  LPCH-ADDON-ORIG-REGPY
             MOVE "03 "                  TO WORK-ERRLOG-FIELD
             MOVE "ADDON ORIGPY"             TO WORK-ERRLOG-DESC
             MOVE LPOR-ADDON-ORIG-REGPY  TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-ADDON-ORIG-REGPY  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPARTIALS  NOT =  LPCH-LCPARTIALS          
             MOVE "04 "                  TO WORK-ERRLOG-FIELD
             MOVE "LCPARTIALS"             TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPARTIALS        TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-LCPARTIALS        TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-OLD1ST-PYDATE NOT  = LPCH-OLD1ST-PYDATE 
             MOVE "05 "                  TO WORK-ERRLOG-FIELD
             MOVE "DC OLD 1STPY"               TO WORK-ERRLOG-DESC
             MOVE LPOR-OLD1ST-PYDATE    TO DATE-YYYYMMDD        
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY           TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-OLD1ST-PYDATE    TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY           TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-DEF-TERM       NOT  = LPCH-DEF-TERM           
             MOVE "06 "                  TO WORK-ERRLOG-FIELD
             MOVE "DF TERM "             TO WORK-ERRLOG-DESC
             MOVE LPOR-DEF-TERM         TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-DEF-TERM          TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LCPAID        NOT  = LPCH-LCPAID          
             MOVE "07 "                  TO WORK-ERRLOG-FIELD
             MOVE "LCPAID  "             TO WORK-ERRLOG-DESC
             MOVE LPOR-LCPAID            TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-LCPAID            TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PREPAY-PENALTY NOT  = LPCH-PREPAY-PENALTY     
             MOVE "08 "                  TO WORK-ERRLOG-FIELD
             MOVE "POFF PENALTY"             TO WORK-ERRLOG-DESC
             MOVE LPOR-PREPAY-PENALTY    TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-PREPAY-PENALTY    TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PRORATED-INS-REB NOT  = LPCH-PRORATED-INS-REB   
             MOVE "09 "                  TO WORK-ERRLOG-FIELD
             MOVE "PRO INS REB "             TO WORK-ERRLOG-DESC
             MOVE LPOR-PRORATED-INS-REB  TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-PRORATED-INS-REB  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-PRIOR-POOLID     NOT  = LPCH-PRIOR-POOLID       
             MOVE "10 "                  TO WORK-ERRLOG-FIELD
             MOVE "POOL ID "             TO WORK-ERRLOG-DESC
             MOVE LPOR-PRIOR-POOLID      TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-PRIOR-POOLID      TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INSTYPES         NOT  = LPCH-INSTYPES           
             MOVE "11 "                  TO WORK-ERRLOG-FIELD
             MOVE "ADDON INSTYP"             TO WORK-ERRLOG-DESC
             MOVE LPOR-INSTYPES          TO WORK-ERRLOG-ORG-ALP
             MOVE LPCH-INSTYPES          TO WORK-ERRLOG-CHG-ALP
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-LT-SEQNO         NOT  = LPCH-LT-SEQNO           
             MOVE "12 "                  TO WORK-ERRLOG-FIELD
             MOVE "INS LT SEQ"             TO WORK-ERRLOG-DESC
             MOVE LPOR-LT-SEQNO          TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-LT-SEQNO          TO WORK-ERRLOG-CHG-N0 
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-INTPDTH-DATE    NOT  = LPCH-INTPDTH-DATE
             MOVE "14 "                  TO WORK-ERRLOG-FIELD
             MOVE "INT PDTH"             TO WORK-ERRLOG-DESC
             MOVE LPOR-INTPDTH-DATE      TO DATE-YYYYMMDD         
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-INTPDTH-DATE      TO DATE-YYYYMMDD          
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY            TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-ADDON-LAST-PYAMT   NOT  = LPCH-ADDON-LAST-PYAMT
             MOVE "15 "                  TO WORK-ERRLOG-FIELD
             MOVE "ADDON LASTPY"             TO WORK-ERRLOG-DESC
             MOVE LPOR-ADDON-LAST-PYAMT  TO WORK-ERRLOG-ORG-N2
             MOVE LPCH-ADDON-LAST-PYAMT  TO WORK-ERRLOG-CHG-N2
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-ADDON-INTPDTH-DATE NOT  = LPCH-ADDON-INTPDTH-DATE
             MOVE "16 "                   TO WORK-ERRLOG-FIELD
             MOVE "ADDON INTPD"           TO WORK-ERRLOG-DESC
             MOVE LPOR-ADDON-INTPDTH-DATE TO DATE-YYYYMMDD      
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-ADDON-INTPDTH-DATE TO DATE-YYYYMMDD     
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
             MOVE DATE-MMDDYY             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-FROMCL NOT  = LPCH-FROMCL
             MOVE "17 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRANS FROMCL"          TO WORK-ERRLOG-DESC
             MOVE LPOR-FROMCL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-FROMCL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.

           IF LPOR-TOCL NOT  = LPCH-TOCL
             MOVE "18 "                   TO WORK-ERRLOG-FIELD
             MOVE "TRANS TOCL"          TO WORK-ERRLOG-DESC
             MOVE LPOR-TOCL             TO WORK-ERRLOG-ORG-N0
             MOVE LPCH-TOCL             TO WORK-ERRLOG-CHG-N0
             PERFORM LOG-FIELD-CHANGE.


      *****************************************
       LOG-FIELD-CHANGE SECTION.
      *****************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.


      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO LPMAN3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPMAN3-SYSDATE.
           DISPLAY LPMAN3-SCREEN UPON LPMAIN-WINDOW-HANDLE.
           MOVE LPMAN3-FORM-FIELDS TO WR-BUF.
           MOVE LPMAN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LPMAN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPMAN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPMAN3-HELP--DIR TO HELP-LINK-DIR.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".


      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       ENTER-Y-OR-N.
           MOVE "ENTER: 'Y' OR 'N'" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LP1-FILE.
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMAN3-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LPMAIN-LINK TO FORM-LINK-AREA.
       STOP-RUN.
           EXIT PROGRAM.
