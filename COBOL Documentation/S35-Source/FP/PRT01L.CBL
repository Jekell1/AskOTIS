       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PRT01L.
       DATE-WRITTEN.    013105.
      ******************************************************************
      *
      *  DESCRIPTION:   LASER LABELS
      *                 3 ACROSS, 10 DOWN
      *
      *                  AVERY 5160 LABELS
      *
      *  USES LOAN FILE AS FIRST FILE
      * ONLY PRINTS IN BATCH
      *
      *=================================================================
      * REV:
      * 013105 BAH PREMIER ADVISORY, PR# 2464
      * 060517 BAH MEMO RECORDS WERE NOT BEING CREATED, JUDY
      * 060524 BAH PRINT SECOND LINE OF ADDRESS, AUTO SERVICES PR# 2792
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      * KEC 2016.0216 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * BAH 181112 GLOBAL FMFILE
      * BAH 181127 GLOBAL LMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 20220907 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
 
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FP/PRT01L.CBL S34 06/17/21-10:28:39 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
 
       01  SUB-A                PIC 9(3) COMP-3       VALUE 0.
       01  SUB-D                PIC 9(3) COMP-3       VALUE 0.

       01  C-S-Z                           PIC X(85).
       01  STATE                           PIC XX.
       01  ZIP                             PIC X(10).

       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).
       01  HOLD-FM1-KEY         PIC X(28).
 
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
 
 
       01  TODAYS-DATE          PIC 9(8)     VALUE ZEROES.
       01  SAVE-LM-MEMO.
           03  FILLER                  PIC X(07)     VALUE "FORM # ".
           03  MEMO-FMNO               PIC 9(04)     VALUE ZEROES.
           03  FILLER                  PIC X(03)     VALUE ",  ".
           03  MEMO-DESC               PIC X(25)     VALUE SPACES.
       01  SAVE-LM1-KEY                              VALUE SPACES.
           03  SAVE-LM-BRNO            PIC 9(4).
           03  SAVE-LM-ACCTNO          PIC 9(8).
           03  SAVE-LM-DATE            PIC 9(8).
           03  SAVE-LM-SEQ             PIC 9(3).

       01  LABEL-TABLE        VALUE SPACES.
           03  LABEL-TAB        OCCURS 3.
               05 LABEL-T       OCCURS 10.  
                  07  T-NAME    PIC X(23).
                  07  T-ADR     PIC X(23).
                  07  T-ADR2    PIC X(23).
                  07  T-CSZ     PIC X(23).
                  07  T-NUMBER  PIC X(6).
 
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 61.
           03  SAVE-LN-FEED     PIC 999      VALUE 0.
 
      *********************************************
      *         C O U P O N    B U F F E R
      *********************************************
 
       01  DETAIL-LINE          VALUE " ".
           03  PRINT-AREA       PIC X(80).
 
           03  PRINT-LINE1          REDEFINES PRINT-AREA.
               05  FILLER       PIC X.
               05  D-LABEL1     PIC X(23).
               05  FILLER       PIC X(5).
               05  D-LABEL2     PIC X(23).
               05  FILLER       PIC X(5).
               05  D-LABEL3     PIC X(23).

      * FORCE 10 INES PER INCH (PICA)
       01  END-COMPRESS-PRINT.
           03  FILLER                  PIC X VALUE X"1B".
           03  FILLER                  PIC X VALUE X"26".
           03  FILLER                  PIC X VALUE X"6B".
           03  FILLER                  PIC X VALUE X"30".
           03  FILLER                  PIC X VALUE X"53".
       
      *********************************************
 
       COPY "LIBLP/LASER_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPM_DEF.CPY".
       COPY "LIBLP/LONPM_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPM_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH EXIT-PATHNAME.
 
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".
 
      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
       GET-COMMON.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPM" TO VDU-FORM-NAME.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.

           MOVE SPACES TO IP-PROG PR-REC.

           MOVE IP-FMNO TO FD-FMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "FORM DESCRIPTOR RECORD MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-FD1-FILE.
 
           MOVE "LABEL PRINT IN PROGRESS.........." TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-FM1-FILE.
           PERFORM OPEN-LM1-FILE.

      * THIS OPEN-PR MUST STAY HERE SO BATCH FORM PRINT PIGGYBACK WORKS
           MOVE 0 TO Q-POS C-POS.
           MOVE LONPM-PRU-POS TO P-POS.
           MOVE "N" TO Q-BUF.
           MOVE 00 TO P-BUF.
           PERFORM OPEN-PR-FILE.

      * FORCE 10 INES PER INCH (PICA)
           MOVE END-COMPRESS-PRINT TO PR-REC.
           MOVE 0 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO LABEL-TABLE.
           MOVE 1 TO SUB-A SUB-D.

           MOVE FM-PATH-OWNBR      TO FM-BRNO
                                      QFM2-WBEG-BRNO
                                      QFM2-WEND-BRNO.
           MOVE IP-FMNO            TO FM-FMNO
                                      QFM2-WBEG-FMNO
                                      QFM2-WEND-FMNO.
           MOVE SPACES             TO FM-ZIP
                                      QFM2-WBEG-ZIP.
           MOVE ALL "Z"            TO QFM2-WEND-ZIP.

           PERFORM START-FM2-FILE.
       READ-FM2FILE-NEXT.
           MOVE 0 TO LOAN-KEY.
           PERFORM READ-FM2-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO END-ROUTINE.

           IF FM-FMNO NOT = IP-FMNO
              GO TO END-ROUTINE.

           IF BRANCH NOT = 0
              IF FM-BRNO NOT = BRANCH
                 GO TO READ-FM2FILE-NEXT.

           IF PRINT-REPRINT = "P"
              IF FM-PRINT = "Y"
                 GO TO READ-FM2FILE-NEXT.
           IF PRINT-REPRINT = "R"
              IF FM-PRINT NOT = "Y"
                 GO TO READ-FM2FILE-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO LOAN-KEY.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO READ-FM2FILE-NEXT.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO READ-FM2FILE-NEXT.

           PERFORM LOAD-TABLE.
           
           IF FD-LNMEMO = "Y"
              PERFORM UPDATE-MEMO-RECORD.

           MOVE FM-BRNO TO HOLD-FM2-BRNO.
           MOVE FM-FMNO TO HOLD-FM2-FMNO.
           MOVE FM-ZIP  TO HOLD-FM2-ZIP.
           MOVE FM1-KEY TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.
           IF IO-FG NOT = 0
              GO TO READ-FM2-FILE-NEXT.

           MOVE "Y" TO FM-PRINT.
           PERFORM REWRITE-FM1-FILE.


           IF SUB-D NOT > 10
              GO TO READ-FM2FILE-NEXT.

      * PAGE IS FULL

           PERFORM PAGE-PRINT.

           MOVE 1 TO SUB-A SUB-D.
           MOVE SPACES TO LABEL-TABLE.

           GO TO READ-FM2FILE-NEXT.

      *************************
       LOAD-TABLE SECTION.
      *************************

      * FILE FOLDER LABEL, LNAME, FNAME ONLY
           MOVE SPACES TO T-NAME(SUB-A, SUB-D).
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
               INTO T-NAME(SUB-A, SUB-D).

           MOVE BW-ADR1 TO T-ADR(SUB-A, SUB-D).
           MOVE BW-ADR2 TO T-ADR2(SUB-A, SUB-D).
           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.
           MOVE C-S-Z TO T-CSZ(SUB-A, SUB-D).

           MOVE LN-ACCTNO TO T-NUMBER(SUB-A, SUB-D).

           ADD 1 TO SUB-A.
           IF SUB-A > 3
              MOVE 1 TO SUB-A
              ADD 1 TO SUB-D.

      ********************
       PAGE-PRINT SECTION.
      ********************
           MOVE 1 TO SUB-A SUB-D.
           MOVE 2 TO LN-FEED.
 
       PAGE-PRINT-NEXT-LABEL.
           MOVE T-NAME(1, SUB-D) TO D-LABEL1.
           MOVE T-NAME(2, SUB-D) TO D-LABEL2.
           MOVE T-NAME(3, SUB-D) TO D-LABEL3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE T-ADR(1, SUB-D) TO D-LABEL1.
           MOVE T-ADR(2, SUB-D) TO D-LABEL2.
           MOVE T-ADR(3, SUB-D) TO D-LABEL3.
           PERFORM WRITE-DETAIL-LINE.

           IF T-ADR2(1, SUB-D) NOT = SPACES
              MOVE T-ADR2(1, SUB-D) TO D-LABEL1
           ELSE
              MOVE T-CSZ(1, SUB-D) TO D-LABEL1.
           IF T-ADR2(2, SUB-D) NOT = SPACES
              MOVE T-ADR2(2, SUB-D) TO D-LABEL2
           ELSE
              MOVE T-CSZ(2, SUB-D) TO D-LABEL2.
           IF T-ADR2(3, SUB-D) NOT = SPACES
              MOVE T-ADR2(3, SUB-D) TO D-LABEL3
           ELSE
              MOVE T-CSZ(3, SUB-D) TO D-LABEL3.
           PERFORM WRITE-DETAIL-LINE.

           IF T-ADR2(1, SUB-D) NOT = SPACES
              MOVE T-CSZ(1, SUB-D) TO D-LABEL1.
           IF T-ADR2(2, SUB-D) NOT = SPACES
              MOVE T-CSZ(2, SUB-D) TO D-LABEL2.
           IF T-ADR2(3, SUB-D) NOT = SPACES
              MOVE T-CSZ(3, SUB-D) TO D-LABEL3.
           PERFORM WRITE-DETAIL-LINE.

      *    MOVE T-NUMBER(1, SUB-D) TO D-LABEL1.
      *    MOVE T-NUMBER(2, SUB-D) TO D-LABEL2.
      *    MOVE T-NUMBER(3, SUB-D) TO D-LABEL3.
      *    PERFORM WRITE-DETAIL-LINE.

           MOVE 3 TO LN-FEED.
           ADD 1 TO SUB-D.
           IF SUB-D NOT > 10
              GO TO PAGE-PRINT-NEXT-LABEL.

       PAGE-PRINT-EXIT.
           MOVE LASER-EJECT-PAGE TO PR-REC.
           PERFORM WRITE-PR-REC.
 

      *****************************
       UPDATE-MEMO-RECORD SECTION.
      *****************************

           MOVE SPACES            TO LM-REC.

           MOVE LN-OWNBR          TO SAVE-LM-BRNO.
           MOVE LN-ACCTNO         TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - TODAYS-DATE.
           MOVE 1                 TO SAVE-LM-SEQ.

       CREATE-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-FG = 0 )
              IF ( LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 GO TO UPDATE-MEMO-RECORD-EXIT.

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.

           MOVE FD-FMNO      TO MEMO-FMNO.
           MOVE FD-DESC      TO MEMO-DESC.
           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.


       UPDATE-MEMO-RECORD-EXIT.
           EXIT.

      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           IF LN-FEED > 15
              MOVE SPACES TO PR-REC
              MOVE LN-FEED TO SAVE-LN-FEED
              MOVE 15 TO LN-FEED
              PERFORM WRITE-PR-REC
              SUBTRACT 15 FROM SAVE-LN-FEED GIVING LN-FEED
              GO TO WRITE-DET-LINE.
           IF CONT-PRINT = "1"
              MOVE DETAIL-LINE TO PR-REC
            ELSE
              MOVE DETAIL-LINE TO PR-REC-CUTSHEET.
           PERFORM WRITE-PR-REC.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE PR-REC.
 
 
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
       EXIT-FORM-RESET.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LONPM-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPM-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARA SECTION.
      ***********************************************
       COPY "LIBGB/GETPR.CPY".

      ***************************************
      *        F I L E   I / O              *
      ***************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************
       END-ROUTINE SECTION.
      ********************
           IF T-NAME(1, 1) NOT = SPACES
              PERFORM PAGE-PRINT.

           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.

       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
