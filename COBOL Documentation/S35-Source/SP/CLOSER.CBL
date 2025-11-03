       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLOSER.
       DATE-WRITTEN. 03-31-20
      *****************************************************************
      *
      *  DESCRIPTION   :  CREATE STATUS REPORT FOR OPEN BRANCHES
      *      REPORTS STATUS ON OPEN CASH DRAWERS, SOMEONE POSTING IN BRANCH
      *      UNFINISHED RENEWALS (R), BATCH PAYS IN PROGRESS, BRANCH TRANSFER
      *      IN PROGRESS, GL 1497000 NOT IN BALANCE
      *      NEW FOR WORLD PR #1427              
      *
      * BAH 20200831 REMOVED RESET ON BR1 
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSOP.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDOP.CPY".
       

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CLOSER.CBL S34 06/17/21-12:38:41 >".

      *****************************************************************
      *     F I L E   I F N
      *****************************************************************
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LP01CX_SQL.CPY".
       COPY "LIBLP/LPWSCX.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
      *----------------------------------------------------------------
       01  PR-IFN               PIC XX     VALUE "PR".


      *****************************************************************
      *   MISC WORKERS     *
      *****************************************************************
       01  MESS-LIST            PIC X(5)      VALUE "MESS.".
       01  WARNING-LIST         PIC X(8)      VALUE "WARNING.".
       01  LEGEND               PIC X(70)     VALUE SPACES.

       01  EDIT-LN-ACCTNO       PIC 999999/99.
       01  CLOSING-ERRCD        PIC X VALUE SPACES.
       01  BRANCH               PIC 9(4) VALUE ZEROS.
       01  TRANS-DATE           PIC 9(8).

      *****************************************************************
      *   LINE WORKERS     *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      *****************************************************************
      *     P R I N T   B U F F E R S   *
      *****************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(8).
           03  FILLER           PIC X(28)    VALUE " ".
           03  FILLER           PIC X(40)    VALUE
           "- - - - CHECK  OPEN  BRANCHES  -  - - - ".
           03  FILLER           PIC X(46)     VALUE  " ".


      *****************************************************************
      *    D E T A I L    L I N E S
      *****************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DATE          PIC 99/99/99.
           03  FILLER          PIC X(2).
           03  D-BRHEAD        PIC X(4).
           03  FILLER          PIC X(2).
           03  D-BRNO          PIC 9(4).
           03  FILLER          PIC XX.
           03  D-MSG           PIC X(55).

      ************************************
      * BULK XFER EOD CHECK  - COMMANDS
      *********************************
      * "/$REL/SH/EC /$REL/SH/BULKEOD.SH ".
       01  BULKEOD-CMD.
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL         PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-EC          PIC X(3) VALUE "EC ".
           03  FILLER              PIC X    VALUE "/".
           03  BULKEOD-REL2        PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  BULKEOD-SHELL       PIC X(11) VALUE "BULKEOD.SH ".
           03  BULKEOD-BR          PIC 9(4).
           03  FILLER              PIC X    VALUE " ".
           03  BULKEOD-DATA        PIC X(18).

       01  BULKEOD-PATH.
           03  FILLER          PIC X(16) VALUE
               "/USR/TMP/BULKEOD".
           03  BULKEOD-PATH-BR PIC 9999.


      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  BEG-DATE            PIC 9(8).
           03  END-DATE            PIC 9(8).
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).

       01 VERSION-CONTROL      PIC X VALUE "A".


      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".

       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBCL/CK1497W.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLOSER_DEF.CPY".
       COPY "LIBSP/CLOSER_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLOSER_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE OP-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-CX1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               OP-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    M O D U L E
      *
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLOSER" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM PROCESS-OPEN-BRANCH.

       END-ROUTINE.
           GO TO END-PROGRAM.

      *****************************************************************
       PROCESS-OPEN-BRANCH SECTION.
      *****************************************************************
           MOVE SPACES TO CLOSING-ERRCD.

       NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG = 9 OR RC-STATUS > "A1"
              GO TO EXIT-PROCESS-OPEN-BR.

           IF GROUP-FLAG = "N"
             IF RC-BRNO < BEG-BRANCH
                GO TO NEXT-RC 
             ELSE  
             IF RC-BRNO > END-BRANCH
                GO TO NEXT-RC.

           IF RC-TRANS-DATE < BEG-DATE OR RC-TRANS-DATE > END-DATE
              GO TO NEXT-RC.

           IF GROUP-FLAG = "N"
              GO TO READ-BR.

           MOVE RC-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO NEXT-RC.

       READ-BR.

           MOVE RC-BRNO TO BR-NO BRANCH.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      *----------------------------------------------------------------
       GET-BRANCH-PATHS.
           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              MOVE "BRANCH &&&& HAS UNDEFINED DATA PATH" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
              PERFORM SEND-MESS
              GO TO NEXT-RC.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"           TO  CA-PATH-OVERRIDE 
                                  CX-PATH-OVERRIDE
                                  PD-PATH-OVERRIDE.

           MOVE RC-TRANS-DATE TO TRANS-DATE.

       CHECK-OPEN-BATCHES.
           MOVE RC-TRANS-DATE TO NUM-DATE.
           MOVE NUM-MO TO BPEOD-MO.
           MOVE NUM-DA TO BPEOD-DA.
           MOVE BRANCH TO BPEOD-BR.

           MOVE BPEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE BRANCH TO BPEOD-PATH-BR.
           MOVE BPEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
               MOVE TRANS-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
               MOVE DATE-MMDDYY TO D-DATE
               MOVE "WARNING: BATCH PAYS NOT COMPLETED!!" TO D-MSG
               MOVE "BR #" TO D-BRHEAD
               MOVE BRANCH TO D-BRNO
               PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
          CHECK-BULK-XFERS-IN-PROGRESS.
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO BULKEOD-REL BULKEOD-REL2.
           CALL "C$TOLOWER" USING BULKEOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING BULKEOD-SHELL, VALUE 11.
           MOVE BRANCH TO BULKEOD-BR BULKEOD-PATH-BR.
           MOVE EXT-FILPATH-GLOBAL TO BULKEOD-DATA.
           MOVE BULKEOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           CALL "C$TOLOWER" USING BULKEOD-PATH, VALUE 16.
           MOVE BULKEOD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
               MOVE TRANS-DATE TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
               MOVE DATE-MMDDYY TO D-DATE
               MOVE "WARNING: BULK TRANSFER UPDATE IN PROGRESS!"
               TO D-MSG
               MOVE "BR #" TO D-BRHEAD
               MOVE BRANCH TO D-BRNO
               PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-1497-GL.

           PERFORM CHECK-ALL-PD-RENEWAL.

           PERFORM CHECK-CX-REQUESTS.

           PERFORM CHECK-CASH-DRAWERS.

           GO TO NEXT-RC.

       EXIT-PROCESS-OPEN-BR.
           EXIT.


      *********************************************
       CHECK-1497-GL SECTION.
      *********************************************
           MOVE 0          TO CK1497-AMOUNT.
           MOVE BRANCH     TO CK1497-BRNO.
           MOVE TRANS-DATE TO CK1497-TRANS-DATE.
           MOVE "CL/CK1497" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH    
                                CK1497-WORKERS.
           CANCEL FORM-PATH.
           IF CK1497-AMOUNT NOT = 0
              MOVE "GL ACCOUNT 1497000 DOESNT NET TO 0"
                                      TO D-MSG
              MOVE TRANS-DATE TO D-DATE 
              MOVE "BR #" TO D-BRHEAD
              MOVE BRANCH TO D-BRNO
              MOVE "X" TO CLOSING-ERRCD
              PERFORM WRITE-DETAIL-LINE.


      *********************************************
       CHECK-ALL-PD-RENEWAL SECTION.
      *********************************************
           PERFORM OPEN-PD1-FILE.

           MOVE "R"           TO PD-ID
                                 QPD1-WBEG-ID
                                 QPD1-WEND-ID.
           MOVE BRANCH        TO PD-BRANCH
                                 QPD1-WBEG-BRANCH
                                 QPD1-WEND-BRANCH.
           MOVE 0             TO PD-SOC-SEC
                                 QPD1-WBEG-SOC-SEC.
           MOVE ALL "9"       TO QPD1-WEND-SOC-SEC.

           PERFORM START-PD1-FILE.

       NEXT-PD1.
           PERFORM READ-PD1-FILE-NEXT.

           IF IO-FG = 9 OR (PD-BRANCH NOT = PD-PATH-OWNBR)
              GO TO EXIT-PD-RENEWAL.

           IF (PD-BRANCH NOT = BRANCH) OR (PD-ID NOT = "R")
      *       UNLOCK PD-FILE
              GO TO EXIT-PD-RENEWAL.

           MOVE PD-SOC-SEC TO EDIT-LN-ACCTNO.
           INSPECT EDIT-LN-ACCTNO REPLACING ALL "/" BY "-".

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY. 
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE "BR #" TO  D-BRHEAD.
           MOVE BRANCH TO D-BRNO.
           STRING "RENEWAL LOAN FOR ACCOUNT NO: "
                     EDIT-LN-ACCTNO
                       " NOT COMPLETED!"
                  DELIMITED BY SIZE INTO D-MSG.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-PD1.

       EXIT-PD-RENEWAL.
           PERFORM CLOSE-PD1-FILE.

      *********************************************
        CHECK-CX-REQUESTS SECTION.
      *********************************************
           PERFORM OPEN-CX1-FILE.

           MOVE BRANCH TO CX-BRNO.
           MOVE SPACES TO CX-TYPE.
           MOVE 0      TO CX-SEQNO.

           PERFORM START-CX1-FILE.

       NEXT-CX1.
           PERFORM READ-CX1-FILE-NEXT.
           IF IO-FG = 9 OR (CX-BRNO NOT = CX-PATH-OWNBR)
              GO TO EXIT-CHECKS.

           IF CX-BRNO NOT = BRANCH
               GO TO NEXT-CX1.

      * IF CX-RESCISSION = "U" IT MEANS THESE CHECKS ARE READY TO PRINT
      * AND THEY NEED TO STOP YOU FROM CLOSING THE DAY, THE LOAN WAS
      * 'U'PDATED OUT OF RESCISSION, REGWAN PL# 556
      * THEIR CX-DATE IS BACK WHEN THE LOAN WAS BOOKED
           IF CX-DATE NOT = TRANS-DATE
              IF CX-RESCISSION NOT = "U"
                    GO TO NEXT-CX1.
      * DONT LOOK AT CHECKS FROM LOANS IN RESCISSION PERIOD
           IF CX-RESCISSION = "Y"
              GO TO NEXT-CX1.

           IF CX-TYPE NOT = "LN"
              IF CX-TYPE NOT = "DL"
                 GO TO NEXT-CX1.

      *WORLD DOES NOT WANT TO CREATED AUTO-CHECK, WRITE TO LOG

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY. 
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TRANS-DATE TO D-DATE.
           STRING "UNWRITTEN CHECK FOR ACCOUNT NO: "
                     CX-ACCTNO
                  DELIMITED BY SIZE INTO D-MSG.
           MOVE "BR #" TO D-BRHEAD.
           MOVE BRANCH TO D-BRNO.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-CX1.

       EXIT-CHECKS.
           PERFORM CLOSE-CX1-FILE.

      ***********************************************
       CHECK-CASH-DRAWERS SECTION.
      ***********************************************
           PERFORM OPEN-CA1-FILE.

           MOVE BRANCH     TO CA-BRNO
                              QCA1-WBEG-BRNO
                              QCA1-WEND-BRNO.
           MOVE TRANS-DATE TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE
                                        QCA1-WEND-DATE.
           MOVE 0          TO CA-TILLNO
                              QCA1-WBEG-TILLNO.
           MOVE ALL "9"    TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.
       NEXT-CASH-TILL.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-BRNO NOT = CA-PATH-OWNBR)
              GO TO CHECK-CASH-DRAWERS-COMPLETE.

           IF CA-DATE NOT = TRANS-DATE
              GO TO CHECK-CASH-DRAWERS-COMPLETE.

           IF CA-BRNO NOT = BRANCH
              GO TO CHECK-CASH-DRAWERS-COMPLETE.

           IF CA-BALBY = " "
              MOVE TRANS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY TO D-DATE
              STRING "CASH DRAWER " CA-TILLNO " IS OPEN"
                  DELIMITED BY SIZE INTO D-MSG
              MOVE "BR #" TO D-BRHEAD
              MOVE BRANCH TO D-BRNO
              PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-CASH-TILL.
 
       CHECK-CASH-DRAWERS-COMPLETE.
           EXIT.

      ***********************************************
       INITIALIZATION SECTION.
      ***********************************************

           MOVE CLOSER-QUEUE-POS TO Q-POS.
           MOVE CLOSER-COPIES-POS TO C-POS.
           MOVE CLOSER-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO EXIT-INITIALIZE.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           PERFORM OPEN-RC2-FILE.

           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE 0        TO RC-BRNO
                            QRC2-WBEG-BRNO.
           MOVE ALL "9"  TO QRC2-WEND-BRNO.

           MOVE BEG-DATE TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE DATE-YYYYMMDD.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF VDUSTATUS = "1U"
              GO TO BR-01.            

      *----------------------------------------------------------------

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO BEG-DT.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO BEG-DT.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BEG-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.

           MOVE LEGEND-DATE1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEGDT."    TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEG-DT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       END-DT.

           MOVE LEGEND-DATE2 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ENDDT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEG-DT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO END-DT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEG-DT.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.


      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
             PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER  TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH         TO H-HR.
           MOVE T-MM         TO H-MIN.

           MOVE HEAD1    TO PR-REC.
           MOVE 99       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2    TO PR-REC.
           MOVE 1        TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE 5 TO LN-COUNT.
       END-HEAD-PAGE.
           EXIT.


      *****************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CLOSER-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLOSER-SYSDATE.
           DISPLAY CLOSER-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLOSER-FORM-FIELDS TO WR-BUF.
           MOVE CLOSER-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CLOSER-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLOSER-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLOSER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC SECTION.
      *****************************************************************

       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *****************************************************************
       IO-ROUTINE SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPCXGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPCX1RN.CPY".
       COPY "LIBLP/LPPD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
               MOVE "SP/UTMENU" TO FORM-NAM
               MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLOSER-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
