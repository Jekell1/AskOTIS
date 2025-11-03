       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CJMAIN.
       AUTHOR.          MJD.
       DATE-WRITTEN.   113095.
      ******************************************************************
      *                          (MN)
      *
      *  DESCRIPTION:   COLLECTOR JOURNAL FILE MAINTENANCE
      *
      * REV:
      * JFF 121595 ADDED FIELDS: CJ-CON-STAT,CJ-REC-STAT,
      *            CJ-FORM-NO,CJ-PROMISE-DATE,CJ-PROMISE-AMT,
      *            CJ-BEG-LM-SEQ,CJ-END-LM-SEQ.
      * JFF 122195 ADDED FIELDS: CJ-CURR-BAL,CJ-PAST-DUE.
      * KEC 032196 MADE CHANGES ON READING THE CD-FILE AND THE CODE-SCAN
      * BLV 010111 ADD CJ-POSTING-BR, TFC PR# 60
      * BLV 030207 ADD CJ-QUEUE, QUEUE BEING WORKED, REGACC PR# 00023
      * GLB 030219 ADD WI/CJSCAN WINDOW; MODIFY F-KEYS AND ORIGINAL SEARCH
      * GLB 030219 PERMIT INVALID COLLECTOR ID; OKAY TO DO?
      * MJD 131209 A30 CONVERSION COMPLETED
      * KEC 2016.0309 CHANGED CD-ACTION???? TO BE CD-ACTION-????.
      *         CHANGED CD-AC-RE-REC TO BE SPLIT INTO CD-AT-REC &
      *         CD-RE-REC.  FIELDS CD-??-ACTIONCD, CD-??-MEMO-REQUIRED,
      *         CD-??-SUPERVISOR-ONLY ARE IDENTICAL (LOCATION, NAME,
      *         SIZE) IN SITUATIONS WERE NOT SURE WHICH TYPE CD-REC
      *         WAS READ.
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2016.1004 FIXED HOLD-KEY FIELDS TO BE HOLD-CJ1-KEY FIELDS.
      *  CS 170418 ACTIVATE 8 DIGIT DATES 
      *  CS 190118 GLOBAL CJFILE
      *  CS 190311 TEMPORARY MADE THE F5/F6 SCAN WORK BECAUSE I NEEDED
      *            TO FIND ACCOUNT AND DIDNT WANT TO START EVERYTIME AT THE
      *            BEGINNING OF FILE. WILL NEED CHANGED AFTER WE CHANGE
      *            SCREEN ENTRY SIZE TO MATCH THE KEY
      *  CS 191112 ****AS IN A15 ONLY CAN ACCESS YOUR BRANCH****
      *  CS 191113 FORCE THE BRANCH TO EXT-FILPATH-BRNO, HOWEVER IF YOU 
      *            ENTER A DIFFERENT BRNO IN SCANKEY, THE START OF CJFILE
      *            WILL ALTER BACK TO CJ-PATH-OWNBR, SO IF I ENTER BR10
      *            AND MY BRANCH IS 1, I WILL SEE BRANCH 1 DATA, I ASKED
      *            KEN IF I NEEDED TO ISSUE A MESSAGE, HE SAYS NO.
      *            LOCAL IN YOU CAN ONLY VIEW YOUR BRANCH
      *            SAVED CJMAIN.191118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/CJMAIN.CBL S35 04/24/24-08:31:16 >".
      *****************************************************************
      *          PROGRAM WORKERS
      *****************************************************************
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ORIGINAL-CJ-REC             PIC X(CJ-SIZE).
       01  CHANGED-CJ-REC              PIC X(CJ-SIZE).

      ******************************************************************
       01  ERRCD             PIC X     VALUE SPACES.
       01  ONE               PIC S9.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.

       01  ACCT-DESC         PIC X(40) VALUE SPACES.

      *----------------------------------------------------------------
      * CODE-FOUND     : CODE IS " " OR EXISTS IN CD-FILE.
      * CODE-NOT FOUND : CODE DOES NOT EXIST IN CD-FILE.
      *----------------------------------------------------------------
       01  CODE-FOUND        PIC X     VALUE "Y".
           88 CODE-NOT-FOUND           VALUE "N".

      *----------------------------------------------------------------
      * HOLD KEY  USED TO STORE CJ1-KEY WHILE CLEARING NEW RECORD.
      *----------------------------------------------------------------
       01  HOLD-CJ1-KEY                PIC X(21).
       01  FILLER            REDEFINES HOLD-CJ1-KEY.
           03  HOLD-CJ-BRNO            PIC 9(4).
           03  HOLD-CJ-COLLID          PIC XXX.
           03  HOLD-CJ-DATE            PIC 9(8).
           03  HOLD-CJ-START-TIME      PIC 9(4).
           03  HOLD-CJ-SEQNO           PIC 99.
       01  HOLD-SCAN-ENTRY.
           03  HOLD-SCAN-BRNO            PIC 9(4).
           03  HOLD-SCAN-COLLID          PIC XXX.
           03  HOLD-SCAN-DATE            PIC 9(6).
           03  HOLD-SCAN-START-TIME      PIC 9(4).
           03  HOLD-SCAN-SEQNO           PIC 99.

      *----------------------------------------------------------------
      *  DISPLAY TIME  USED TO TEST FOR VALID HOURS AND MINUTES
      *----------------------------------------------------------------
       01  DISPLAY-TIME      PIC 9(4).
       01  FILLER REDEFINES DISPLAY-TIME.
           03  DISPLAY-HR    PIC 99.
           03  DISPLAY-MN    PIC 99.

      *****************************************************************
      *          SCREEN DISPLAY BUFFER
      *
      *  F01  - ACCOUNT NUMBER.
      *  F02  - END TIME
      *  F03  - ACTIVITY CODE
      *  F031 - ACTIVITY CODE DESCRIPTION
      *  F04  - RESULT CODE
      *  F041 - RESULT CODE DESCRIPTION
      *  F05  - ACTION CODE
      *  F051 - ACTION CODE DESCRIPTION
      *  F06  - NEW COLLECTOR CODE
      *  F061 - NEW COLLECTOR CODE DESCRIPTION
      *  F07  - CONTRACTUAL STATUS
      *  F08  - RECENCY STATUS
      *  F09  - FORM NUMBER
      *  F10  - PROMISE DATE 1
      *  F11  - PROMISE AMOUNT
      *  F12  - PROMISE DATE 2
      *  F13  - BEGINNING LOAN MEMO SEQ
      *  F14  - ENDING LOAN MEMO SEQ
      *  F15  - CURRENT BALANCE
      *  F16  - PAST DUE
      *  F17  - POSTING BRANCH
      *  F18  - QUEUE
      *****************************************************************
       01  DISPLAY-BUF       VALUE SPACES.
           03  F01           PIC Z(8).
           03  F02           PIC ZZZ9.
           03  F03           PIC X(3).
           03  F031          PIC X(25).
           03  F04           PIC X(3).
           03  F041          PIC X(25).
           03  F05           PIC X(4).
           03  F051          PIC X(25).
           03  F06           PIC X(3).
           03  F061          PIC X(25).
           03  F07           PIC X(3).
           03  F08           PIC X(3).
           03  F09           PIC ZZ9.
           03  F10           PIC 99/99/99.
           03  F11           PIC ZZZ,ZZ9.99.
           03  F12           PIC 99/99/99.
           03  F13           PIC Z9.
           03  F14           PIC Z9.
           03  F15           PIC ZZZ,ZZ9.99.
           03  F16           PIC ZZZ,ZZ9.99.
           03  F17           PIC ZZZ9.
           03  F18           PIC X(3).

       01  DISPLAY-LIST.
           03  FILLER        PIC X(44) VALUE
           "010 020 030 031 040 041 050 051 060 061 070 ".
           03  FILLER        PIC X(44) VALUE
           "080 090 100 110 120 130 140 150 160 170 180.".

      *****************************************************************
      *       ELEMENT SPECIFICATIONS
      *****************************************************************
       01  MAX               PIC 9(2)    VALUE 18.
       01  SPEC-TABLE.
      * ACCOUNT
           03  FILLER        PIC X(8) VALUE "ENNN080 ".
      * END TIME
           03  FILLER        PIC X(8) VALUE "ENNN040 ".
      * ACTIVITY CODE
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * RESULT CODE
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * ACTION CODE
           03  FILLER        PIC X(8) VALUE "E  A040 ".
      * NEW COLLECTOR ID CODE
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * CONTRACTUAL STATUS
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * RECENCY STATUS
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      * FORM NUMBER
           03  FILLER        PIC X(8) VALUE "E  N030 ".
      * PROMISE DATE 1
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * PROMISE AMOUNT
           03  FILLER        PIC X(8) VALUE "E  N062 ".
      * PROMISE DATE 2
           03  FILLER        PIC X(8) VALUE "ERNM060 ".
      * BEGINNING LOAN MEMO SEQUENCE
           03  FILLER        PIC X(8) VALUE "E  N020 ".
      * ENDING LOAN MEMO SEQUENCE
           03  FILLER        PIC X(8) VALUE "E  N020 ".
      * CURRENT BALANCE
           03  FILLER        PIC X(8) VALUE "E  N062 ".
      * PAST DUE
           03  FILLER        PIC X(8) VALUE "E  N062 ".
      * POSTING BRANCH (NORMAL = 0, BRANCH # IF GLOBAL QUEUING)
           03  FILLER        PIC X(8) VALUE "E  N040 ".
      * QUEUE BEING WORKED
           03  FILLER        PIC X(8) VALUE "E  A030 ".
      *----------------------------------------------------------------

       01  SPEC-TAB          REDEFINES SPEC-TABLE.
           03  SPEC-REC      OCCURS 18 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/CJSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01 CJMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CJMAIN_DEF.CPY".
       COPY "LIBMN/CJMAIN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/CJMAIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
      *          MAIN PROGRAM MODULE
      *****************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "CJMAIN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CJMAIN-WINDOW-HANDLE.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE SPACES TO CJ-REC
              MOVE 0 TO CJ-BRNO CJ-DATE CJ-START-TIME CJ-SEQNO
              MOVE SPACES TO HOLD-CJ-COLLID
              MOVE 0 TO HOLD-CJ-BRNO HOLD-CJ-DATE
                        HOLD-CJ-START-TIME HOLD-CJ-SEQNO.

           PERFORM FORM-RESET.

           PERFORM REC-CJ-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      *****************************************************************
       REC-CJ-MODULE SECTION.
      *****************************************************************
       REC-CJ-MODULE-NO.
           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           PERFORM FORM-RESET.

           MOVE SPACES TO HOLD-CJ-COLLID.
           MOVE 0      TO HOLD-CJ-DATE
                          HOLD-CJ-START-TIME
                          HOLD-CJ-SEQNO.



      *----------------------------------------------------------------
      *ENTER-BRANCH.
      *    MOVE "F1-CLEAR, F2-WINDOW SCAN, F5-NEXT SCAN, F7-EXIT"
      *       TO MESS.
      *    PERFORM SEND-LEGEND.
      *THERE IS A BRANCH PROMPT ON THE SCREEN BUT YOU CAN ONLY SEE YOUR OWN
      *DATA, CONFUSING BECAUSE WHEN YOU ENTER A DIFFERENT BRANCH NUMBER IT
      *WANTS TO CREATE A NEW RECORD FOR EXTERNAL-FILPATH-BRNO.
      *SO SENDING THE BRANCH.


       SEND-BRANCH.
           MOVE "SNNN040BR1." TO VDU.
           MOVE EXT-FILPATH-BRNO TO CJ-BRNO,VDUNUM0.
           PERFORM SCREENIO.
          
           MOVE VDUNUM0 TO HOLD-CJ-BRNO.
           PERFORM VERIFY-BRNO.
           MOVE "S  A350BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
           IF CODE-NOT-FOUND
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CJ-MODULE.

      *----------------------------------------------------------------
       ENTER-COLLECTOR.

           MOVE "F1-CLEAR,F2-WINDOW,F5-SCAN,F6-COLLECTORS,F7-EXIT"
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN A030COLL." TO VDU.
           PERFORM SCREENIO.

      *  F1 =  CLEAR SCREEN
           IF ( VDUSTATUS = "10" )
              GO TO REC-CJ-MODULE-NO.

      *  F2 =  NEW WINDOW SCAN
           IF ( VDUSTATUS = "12" )
              MOVE 0 TO CJ-SEQNO
              PERFORM CJFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-CJ-MODULE
              ELSE
                 GO TO READ-CJFILE.

      *  F5 = SCREEN SCAN
           IF ( VDUSTATUS = "1:" )
              MOVE SPACES TO HOLD-CJ-COLLID
              MOVE 0 TO
                     HOLD-CJ-DATE
                     HOLD-CJ-START-TIME
                     HOLD-CJ-SEQNO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CJ-MODULE.

      *  F6 =  CODE SCAN
           IF ( VDUSTATUS =  "1<" )
              MOVE 99 TO ELE
              PERFORM CODE-SCAN.

      *  F7 = EXIT
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CJ-MODULE.

      *    IF ( VDUSTATUS =  "1U" )
      *       GO TO ENTER-BRANCH.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-COLLECTOR.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-COLLECTOR.

           IF VDUBUF = SPACES
              MOVE "S  A000COLL." TO VDU
              MOVE HOLD-CJ-COLLID TO VDUBUF
              PERFORM SCREENIO.

           MOVE VDUBUF   TO HOLD-CJ-COLLID CJ-COLLID CDB-CODE.
      D    STOP MESS.
           PERFORM VERIFY-COLLECTOR-CODE.
           MOVE "S  A200COLLDESC." TO VDU.
           MOVE CDB-DESC           TO VDUBUF.
           PERFORM SCREENIO.

      *  GLB  02/19/03
      *    IF ( CODE-NOT-FOUND )
      *       GO TO ENTER-COLLECTOR.

      *----------------------------------------------------------------
       ENTER-DATE.
           MOVE "F1-CLEAR,F2-WINDOW,F5-SCAN,F7-EXIT, ENTER DATE:"
              TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DATE." TO VDU.
           PERFORM SCREENIO.

      *  F1 =  CLEAR SCREEN
           IF ( VDUSTATUS = "10" )
              GO TO REC-CJ-MODULE-NO.

      *  F2 =  NEW WINDOW SCAN
           IF ( VDUSTATUS = "12" )
              MOVE 0 TO CJ-SEQNO
              PERFORM CJFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-CJ-MODULE
              ELSE
                 GO TO READ-CJFILE.

      *  F5 = SCREEN SCAN
           IF ( VDUSTATUS = "1:" )
              MOVE 0 TO
                     HOLD-CJ-DATE
                     HOLD-CJ-START-TIME
                     HOLD-CJ-SEQNO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CJ-MODULE.

      *  F6 =  CODE SCAN
      *    IF ( VDUSTATUS =  "1<" )
      *       MOVE 99 TO ELE
      *       PERFORM CODE-SCAN.

      *  F7 = EXIT
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CJ-MODULE.

           IF ( VDUSTATUS =  "1U" )
              GO TO ENTER-COLLECTOR.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO CJ-DATE HOLD-CJ-DATE.

      *----------------------------------------------------------------
       ENTER-TIME.
           MOVE "F1-CLEAR,F2-WINDOW,F5-SCAN,F7-EXIT,ENTER TIME:"
              TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040TIME1." TO VDU.
           PERFORM SCREENIO.

      *  F1 =  CLEAR SCREEN
           IF ( VDUSTATUS = "10" )
              GO TO REC-CJ-MODULE-NO.

      *  F2 =  NEW WINDOW SCAN
           IF ( VDUSTATUS = "12" )
              MOVE 0 TO CJ-SEQNO
              PERFORM CJFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-CJ-MODULE
              ELSE
                 GO TO READ-CJFILE.

      *  F5 = SCREEN SCAN
           IF ( VDUSTATUS = "1:" )
              MOVE 0 TO
                     HOLD-CJ-START-TIME
                     HOLD-CJ-SEQNO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CJ-MODULE.

      *  F7 = EXIT
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CJ-MODULE.

           IF ( VDUSTATUS =  "1U" )
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TIME.

           IF VDUNUM0 = 0
              MOVE "SN N040TIME1." TO VDU
              MOVE HOLD-CJ-START-TIME TO VDUNUM0
              PERFORM SCREENIO.

           MOVE VDUNUM0 TO DISPLAY-TIME.
           IF DISPLAY-HR < 0 OR DISPLAY-HR > 23
              MOVE "INVALID HOUR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TIME.
           IF DISPLAY-MN < 0 OR DISPLAY-MN > 59
              MOVE "INVALID MINUTE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TIME.

           MOVE DISPLAY-TIME TO CJ-START-TIME
                                HOLD-CJ-START-TIME.

      *----------------------------------------------------------------
       ENTER-SEQ.
           MOVE "F1-CLEAR,F2-WINDOW,F5-SCAN,F7-EXIT,ENTER SEQ#:"
              TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020SEQNO." TO VDU.
           PERFORM SCREENIO.

      *  F1 =  CLEAR SCREEN
           IF ( VDUSTATUS = "10" )
              GO TO REC-CJ-MODULE-NO.

      *  F2 =  NEW WINDOW SCAN
           IF ( VDUSTATUS = "12" )
              MOVE 0 TO CJ-SEQNO
              PERFORM CJFILE-SCAN
              IF IO-FG = 9
                 GO TO REC-CJ-MODULE
              ELSE
                 GO TO READ-CJFILE.

      *  F5 = SCREEN SCAN
           IF ( VDUSTATUS = "1:" )
              MOVE 0 TO
                     CJ-SEQNO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-CJ-MODULE.

      *  F7 = EXIT
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CJ-MODULE.

           IF ( VDUSTATUS =  "1U" )
              GO TO ENTER-TIME.

           MOVE VDUNUM0 TO CJ-SEQNO 
                          HOLD-CJ-SEQNO.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

      *----------------------------------------------------------------
       READ-CJFILE.
           PERFORM OPEN-CJ1-FILE.
           MOVE HOLD-CJ1-KEY TO CJ1-KEY.
           PERFORM READ-CJ1-FILE.
           PERFORM CLOSE-CJ1-FILE.
           IF IO-FG = 0
              MOVE CJ-REC TO ORIGINAL-CJ-REC
              PERFORM DISPLAY-KEY-FIELDS
              PERFORM DISPLAY-FIELDS
           ELSE
              PERFORM NEW-RECORD-SETUP.
       END-REC-CJ-MODULE.
           EXIT.

      *****************************************************************
      *    ENTER SCANKEY ROUTINES
      *
      *****************************************************************
       ENTER-SCANKEY SECTION.
           MOVE "F1-RESET, F6-NEXT SCREEN SCAN:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 0 TO FIRST-SCAN.

           PERFORM OPEN-CJ1-FILE.

       ENTER-SCANKEY-KEY.
           MOVE "E  A190SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.
           IF VDUSTATUS = "1<"
              GO TO ENTER-SCANKEY-READ-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 GO TO ENTER-SCANKEY-CLOSE
              ELSE
                 GO TO ENTER-SCANKEY-KEY.

      *----------------------------------------------------------------
      * TEMP KEY IS USED TO COPY VDUBUF INTO CJ1-KEY WHILE
      *           MAINTAINING COMP-3 INTEGRITY.
      *----------------------------------------------------------------
      *    MOVE VDUBUF TO CJ1-KEY.
      *TEMPORARY UNTIL WE CHANGE THE SCREEN ENTRY TO SIZE OF KEY

           MOVE VDUBUF TO HOLD-SCAN-ENTRY.

      *IF HOLD-SCAN-BRNO NOT = EXT-FILPATH-BRANCH, CJ-BRNO IS GETTING
      *RESET ON START OF CJ, SO IF YOU ENTERED 0010 IN SCANKEY FOR BRANCH
      *PORTION, YOU ARE SEEING DATA FOR YOUR BRANCH

           MOVE HOLD-SCAN-BRNO        TO CJ-BRNO.
           MOVE HOLD-SCAN-COLLID      TO CJ-COLLID.
           MOVE HOLD-SCAN-DATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD         TO CJ-DATE.
           MOVE HOLD-SCAN-START-TIME  TO CJ-START-TIME.
           MOVE HOLD-SCAN-SEQNO       TO CJ-SEQNO.
           
           MOVE CJ1-KEY TO HOLD-CJ1-KEY.
           
           IF CJ-DATE = 0
              MOVE 19000101 TO CJ-DATE.

           PERFORM START-CJ1-FILE.

       ENTER-SCANKEY-READ-NEXT.

      D    STOP MESS.
           IF FIRST-SCAN = 0
              MOVE HOLD-CJ-BRNO       TO CJ-BRNO
              MOVE HOLD-CJ-COLLID     TO CJ-COLLID
              MOVE HOLD-CJ-DATE       TO CJ-DATE 
              PERFORM CHECK-CJ-DATE-FOR-ZERO
              MOVE HOLD-CJ-START-TIME TO CJ-START-TIME
              MOVE HOLD-CJ-SEQNO      TO CJ-SEQNO
      * DATE IS 0, DO NOT USE DATE IN THIS START
              PERFORM START-CJ1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-CJ1-FILE-NEXT.
           IF IO-FG = 9 OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO ENTER-SCANKEY-CLOSE.

      *    UNLOCK CJ-FILE.
           MOVE CJ-REC TO ORIGINAL-CJ-REC.
           MOVE CJ1-KEY TO HOLD-CJ1-KEY.

           PERFORM DISPLAY-KEY-FIELDS.
           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-SCANKEY-KEY.

       ENTER-SCANKEY-CLOSE.
           MOVE 0 TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       ENTER-SCANKEY-END.
           EXIT.

      *****************************************************************
      *       ENTRY-MODULE
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      *****************************************************************
       ENTRY-MODULE SECTION.
      *-----------------------------------------------------------------
       BEGIN-ENTRY.

           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

      *-----------------------------------------------------------------
       CREATE-ROUTINE.

           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE "F1, 'DELETE', ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                    GO TO MODIFY-ROUTINE.

           PERFORM DISPLAY-MSG.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      * TEST UP ARROW
      *----------------------------------------------------------------
       TEST-F4.

           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      * TEST DOWN ARROW
      *----------------------------------------------------------------
       TEST-F5.

           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

      *----------------------------------------------------------------
      * ONLY ACTIVATE F6 SCAN INTERRUPT IF ENTERING:
      *      ELE = 3 - ACTIVITY CODE.
      *            4 - RESULT CODE.
      *            5 - ACTION CODE.
      *            6 - COLLECTOR CODE.
      *----------------------------------------------------------------
       TEST-F6.

           IF ( ELE = 3 OR 4 OR 5 OR 6 )
              IF ( VDUSTATUS = "1<" )
                 PERFORM CODE-SCAN
                 IF ( IO-FG NOT = 0 )
                    GO TO ENTER-ELE
                 ELSE
                    MOVE 1 TO ONE
                    GO TO GOTO-FIELD.

      *----------------------------------------------------------------
      * TEST CLEAR
      *----------------------------------------------------------------
       TEST-F1.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

      *================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *================================================================
       F-01.
           MOVE VDUNUM0 TO CJ-ACCTNO LN-ACCTNO.
           PERFORM VERIFY-ACCOUNT.
           MOVE "S  A200011." TO VDU
           MOVE ACCT-DESC TO VDUBUF
           PERFORM SCREENIO

           IF CODE-NOT-FOUND
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM0 TO DISPLAY-TIME.

           IF DISPLAY-HR < 0 OR DISPLAY-HR > 23
              MOVE "INVALID HOUR" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF DISPLAY-MN < 0 OR DISPLAY-MN > 59
              MOVE "INVALID MINUTE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE DISPLAY-TIME TO CJ-END-TIME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO CJ-ACTIVITY-CD CD-CODE.
           PERFORM VERIFY-ACTIVITY-CODE.
           MOVE "S  A200031." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF CODE-NOT-FOUND
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO CJ-RESULT-CD CD-CODE.
           PERFORM VERIFY-RESULT-CODE.
           MOVE "S  A200041." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF CODE-NOT-FOUND
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO CJ-ACTION-CD CD-CODE.
           PERFORM VERIFY-ACTION-CODE.
           MOVE "S  A200051." TO VDU.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF CODE-NOT-FOUND
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO CJ-NEW-COLLID CDB-CODE.
           PERFORM VERIFY-COLLECTOR-CODE.
           MOVE "S  A200061." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF ( CODE-NOT-FOUND )
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO CJ-CON-STAT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO CJ-REC-STAT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO CJ-FORM-NO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDU-DATE-YYYYMMDD TO CJ-PROMISE-DATE1.        
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO CJ-PROMISE-AMT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDU-DATE-YYYYMMDD TO CJ-PROMISE-DATE2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM0 TO CJ-BEG-LM-SEQ.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM0 TO CJ-END-LM-SEQ.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO CJ-CUR-BAL.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO CJ-PAST-DUE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO CJ-POSTING-BR.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-18.
           MOVE VDUBUF TO CJ-QUEUE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************************
       WRITE-MODULE SECTION.
      *****************************************************************
           PERFORM OPEN-CJ1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.
           MOVE CJ-REC TO CHANGED-CJ-REC.
           IF ORIGINAL-CJ-REC = CHANGED-CJ-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-CJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CJ-REC NOT = ORIGINAL-CJ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-CJ-REC TO CJ-REC.
           PERFORM REWRITE-CJ1-FILE.
           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-CJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-CJ1-FILE.

      *****************************************************************
       DELETE-MODULE SECTION.
      *****************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CJ1-FILE.
           PERFORM READ-CJ1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CJ-REC NOT = ORIGINAL-CJ-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-CJ1-FILE.
       END-DELETE-MODULE.
           PERFORM CLOSE-CJ1-FILE.

      *****************************************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-CJ-FILE.
           MOVE HOLD-CJ1-KEY TO CJ1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-KEY-FIELDS SECTION.
      *****************************************************************
           MOVE "SN N040BR1." TO VDU.
           MOVE CJ-BRNO TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-BRNO.

           MOVE "S  A350BRDESC." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN A040COLL." TO VDU.
           MOVE CJ-COLLID TO VDUBUF CDB-CODE.
           PERFORM SCREENIO.

           PERFORM VERIFY-COLLECTOR-CODE.
           MOVE "S  A200COLLDESC." TO VDU.
           MOVE CDB-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN A080DATE." TO VDU.
           MOVE CJ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SN N040TIME1." TO VDU.
           MOVE CJ-START-TIME TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020SEQNO." TO VDU.
           MOVE CJ-SEQNO TO VDUNUM0.
           PERFORM SCREENIO.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE CJ-ACCTNO TO F01 LN-ACCTNO.
           PERFORM VERIFY-ACCOUNT.
           MOVE "S  A200011." TO VDU.
           MOVE ACCT-DESC TO VDUBUF.
           PERFORM SCREENIO.
       
           MOVE CJ-END-TIME TO F02.
       
           MOVE CJ-ACTIVITY-CD TO F03 CD-CODE.
           PERFORM VERIFY-ACTIVITY-CODE.
           MOVE CD-DESC TO F031.
       
           MOVE CJ-RESULT-CD TO F04 CD-CODE.
           PERFORM VERIFY-RESULT-CODE.
           MOVE CD-DESC TO F041.
       
           MOVE CJ-ACTION-CD TO F05 CD-CODE.
           PERFORM VERIFY-ACTION-CODE.
           MOVE CD-DESC TO F051.
       
           MOVE CJ-NEW-COLLID TO F06 CDB-CODE.
           PERFORM VERIFY-COLLECTOR-CODE.
           MOVE CDB-DESC TO F061.
       
           MOVE CJ-CON-STAT TO F07.
           MOVE CJ-REC-STAT TO F08.
           MOVE CJ-FORM-NO  TO F09.
       
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F10.
       
           MOVE CJ-PROMISE-AMT TO F11.
       
           MOVE CJ-PROMISE-DATE2 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO F12.
       
           MOVE CJ-BEG-LM-SEQ TO F13.
           MOVE CJ-END-LM-SEQ TO F14.
           MOVE CJ-CUR-BAL    TO F15.
           MOVE CJ-PAST-DUE   TO F16.
           MOVE CJ-POSTING-BR TO F17.
           MOVE CJ-QUEUE      TO F18.
       
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       CJFILE-SCAN SECTION.
      ******************************************************************

           MOVE HOLD-CJ1-KEY    TO CJSCANW-CJ1-KEY.

           MOVE "WI/CJSCAN"     TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CJSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF CJSCANW-STATUS NOT = "00"
              MOVE 9 TO IO-FG
           ELSE
              MOVE 0 TO IO-FG
              MOVE CJSCANW-BRNO       TO HOLD-CJ-BRNO CJ-BRNO
              MOVE CJSCANW-COLLID     TO HOLD-CJ-COLLID CJ-COLLID
              MOVE CJSCANW-DATE       TO HOLD-CJ-DATE CJ-DATE
              MOVE CJSCANW-START-TIME TO HOLD-CJ-START-TIME
                                              CJ-START-TIME
              MOVE CJSCANW-SEQNO       TO HOLD-CJ-SEQNO CJ-SEQNO.


      ******************************************************************
       VERIFY-COLLECTOR-CODE SECTION.
      ******************************************************************
           MOVE "Y" TO CODE-FOUND.

           IF ( CDB-CODE = SPACES )
              MOVE SPACES TO CDB-DESC
           ELSE
              MOVE "CN"    TO CDB-TYPE
              IF CJ-POSTING-BR = 0 OR 2020
                 MOVE HOLD-CJ-BRNO TO CDB-BRANCH
              ELSE
                 MOVE 0 TO CDB-BRANCH
              END-IF
              PERFORM OPEN-CDB1-FILE
              PERFORM READ-CDB1-FILE
              PERFORM CLOSE-CDB1-FILE
              IF ( IO-FG NOT = 0 )
                 MOVE "* NOT ON FILE *" TO CDB-DESC
                 MOVE "N" TO CODE-FOUND.

      ******************************************************************
       VERIFY-ACTIVITY-CODE SECTION.
      ******************************************************************

           MOVE "Y"    TO CODE-FOUND.

           IF ( CD-CODE = SPACES )
              MOVE " " TO CD-DESC
           ELSE
              MOVE "AT" TO CD-TYPE
              PERFORM GET-CD
              IF ( IO-FG NOT = 0 )
                 MOVE "N" TO CODE-FOUND.

      ******************************************************************
       VERIFY-RESULT-CODE SECTION.
      ******************************************************************

           MOVE "Y"    TO CODE-FOUND.

           IF ( CD-CODE = " " )
              MOVE " " TO CD-DESC
           ELSE
              MOVE "RE" TO CD-TYPE
              PERFORM GET-CD
              IF ( IO-FG NOT = 0 )
                 MOVE "N" TO CODE-FOUND.

      ******************************************************************
       VERIFY-ACTION-CODE SECTION.
      ******************************************************************

           MOVE "Y" TO CODE-FOUND.

           IF ( CD-CODE = " " )
              MOVE " " TO CD-DESC
           ELSE
              MOVE "AC" TO CD-TYPE
              PERFORM GET-CD
              IF ( IO-FG NOT = 0 )
                 MOVE "N" TO CODE-FOUND.

      ******************************************************************
      *
      *    C O D E - S C A N
      *
      *     ELE : 3 = ACTIVITY CODE SCAN.
      *           4 = RESULT CODE SCAN.
      *           5 = ACTION CODE SCAN.
      *           6 = COLLECTOR CODE SCAN.
      *          99 = COLLECTOR CODE SCAN FOR KEY ENTRY.
      ******************************************************************
       CODE-SCAN SECTION.
      D    STOP MESS.

           IF ( ELE = 3 )
              MOVE 41     TO CDSCAN-BEGIN-X
              MOVE "AT"   TO CDSCAN-TYPE
              MOVE SPACES TO CDSCAN-CODE
           ELSE
           IF ( ELE = 4 )
              MOVE 41     TO CDSCAN-BEGIN-X
              MOVE "RE"   TO CDSCAN-TYPE
              MOVE SPACES TO CDSCAN-CODE
           ELSE
           IF ( ELE = 5 )
              MOVE 41     TO CDSCAN-BEGIN-X
              MOVE "AC"   TO CDSCAN-TYPE
              MOVE SPACES TO CDSCAN-CODE
           ELSE
           IF ( ELE = 6 OR 99)
              PERFORM CODE-B-SCAN
              GO TO EXIT-CODE-SCAN.

           MOVE 8   TO CDSCAN-BEGIN-Y.
           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS     NOT = "00"    ) OR
              (( CDSCAN-TYPE = "AC") AND (CDSCAN-CODE = SPACES)) OR
              (( CDSCAN-TYPE = "AT") AND (CDSCAN-CODE = SPACES)) OR
              (( CDSCAN-TYPE = "RE") AND (CDSCAN-CODE = SPACES))
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           IF ( ELE = 5 )
              MOVE "S  A040  0." TO VDU
              MOVE ELE TO VDUROW
           ELSE
              MOVE "S  A030  0." TO VDU
              MOVE ELE TO VDUROW.

           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CDSCAN-CODE TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ***********************************************************
       CODE-B-SCAN SECTION.
      ***********************************************************
           MOVE 08           TO CDBSCN-BEGIN-Y.
           MOVE 10           TO CDBSCN-BEGIN-X.
           MOVE "CN"         TO CDBSCN-TYPE.
           MOVE HOLD-CJ-BRNO TO CDBSCN-BRANCH.
           MOVE SPACES       TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS     NOT = "00"    ) OR
              (( CDBSCN-TYPE = "CN") AND (CDBSCN-CODE = SPACES)) 
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-B-SCAN.

           IF ( ELE = 99 )
              MOVE "S  A040COLL." TO VDU
           ELSE
           IF ( ELE = 6 )
              MOVE "S  A040  0." TO VDU
              MOVE ELE TO VDUROW.

           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO IO-FG.
           MOVE CDBSCN-CODE TO VDUBUF.

       EXIT-CODE-B-SCAN.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO CJMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJMAIN-SYSDATE.
           DISPLAY SPACES UPON CJMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CJMAIN-SCREEN UPON CJMAIN-WINDOW-HANDLE.
           MOVE CJMAIN-FORM-FIELDS TO WR-BUF.
           MOVE CJMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CJMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJMAIN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/CJMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBSP/SPCJCL.CPY".

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       CHECK-CJ-DATE-FOR-ZERO.
           IF CJ-DATE = 0
              MOVE 19000101 TO CJ-DATE.
       DISPLAY-MSG.
           IF ELE = 1
              MOVE "F1-CLEAR, ENTER ACCOUNT NUMBER:" TO MESS
           ELSE
           IF ELE = 2
              MOVE "F1-CLEAR, ENTER ENDING TIME:" TO MESS
           ELSE
           IF ELE = 3
              MOVE "F1-CLEAR, F6-SCAN, ENTER ACTIVITY CODE:" TO MESS
           ELSE
           IF ELE = 4
              MOVE "F1-CLEAR, F6-SCAN, ENTER RESULT CODE:" TO MESS
           ELSE
           IF ELE = 5
              MOVE "F1-CLEAR, F6-SCAN, ENTER ACTION CODE:" TO MESS
           ELSE
           IF ELE = 6
              MOVE "F1-CLEAR, F6-SCAN, ENTER NEW COLLECTOR ID:" TO MESS
           ELSE
              MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       
       VERIFY-BRNO.
           MOVE "Y" TO CODE-FOUND.
           PERFORM OPEN-BR-FILE.
           MOVE HOLD-CJ-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO CODE-FOUND
              MOVE "* NOT ON FILE *" TO BR-NAME.
       
       VERIFY-ACCOUNT.
           MOVE "Y" TO CODE-FOUND.
           PERFORM GET-LN
           IF IO-FG NOT = 0
              MOVE "N" TO CODE-FOUND
              MOVE "* NOT ON FILE *" TO ACCT-DESC
           ELSE
              MOVE SPACES TO ACCT-DESC.
       
       GET-LN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO CD-DESC.
       
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       
       SEND-LEGEND.
           MOVE "S  A070MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBSP/SPCJ1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
       
      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/CPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY CJMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
