       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ORIGIN.
       DATE-WRITTEN.    040506.
      ************************************************************************
      *                    (FX/ORIGIN.C)
      *
      *  DESCRIPTION: MAKES THE FOLLOWING CHANGED TO THE FILES LISTED:
      *               
      *    LOOP THRU LOANS, READ BORROWER AND TEST BW-STATE AGAINST STATES
      *     ENTERED ON SCREEN, IF IT MATCHES 1, MOVE IN THE ORIGIN THEY 
      *     ENTERED ON SCREEN.
      *
      *  REGACC PR# 126
      *
      * REV:
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *
      *      W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/ORIGIN.CBL S34 05/21/21-15:46:54 >".

       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      *-----------------------------------------------------------------

       01  LN-FEED           PIC 9(2) VALUE ZERO.

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.
       
       01  SUB                PIC 99.
       01  FOUND-MATCH        PIC X VALUE "N".
      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  PL              PIC X.
           03  ZBAL            PIC X.
           03  STATE-CODES.
               05  ST-CODE     PIC XX OCCURS 20.
           03  ORIGIN          PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S 
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/ORIGIN_DEF.CPY".
       COPY "LIBFX/ORIGIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/ORIGIN_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E    D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ORIGIN" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
              GO TO END-ROUTINE.

           IF EXT-ROUTE-BUF = "00"
              STRING "PROGRAM IN PROGRESS......BRNO: " BR-NO
                       DELIMITED BY SIZE INTO MESS-BUF
              PERFORM SEND-LEGEND.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           PERFORM PROCESS-LN-FILE.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.


      ******************************************************************
      *
      *     I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           MOVE 0 TO Q-POS.
           MOVE 0 TO C-POS.
           MOVE 0 TO P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
 
           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    ENTER REPORT PARAMETERS
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ORIGIN.
      *-----------------------------------------------------------------

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO INCLUDE-PL.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO INCLUDE-PL.
      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INCLUDE-PL.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO INCLUDE-PL.

           MOVE VDUBUF TO PL.

           IF PL NOT = "Y" AND NOT = "N"
              GO TO INCLUDE-PL.

       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCLUDE-PL.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL NOT = "Y" AND NOT = "N"
              GO TO ZBALYN.

       ENTER-ST1.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
             MOVE SPACES TO ST-CODE(SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST1.
           MOVE VDUBUF TO ST-CODE(1).

       ENTER-ST2.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST1.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST2.
           MOVE VDUBUF TO ST-CODE(2).

       ENTER-ST3.
           MOVE "E  A020ST3." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST2.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST3.
           MOVE VDUBUF TO ST-CODE(3).
     
       ENTER-ST4.
           MOVE "E  A020ST4." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST3.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST4.
           MOVE VDUBUF TO ST-CODE(4).
     
       ENTER-ST5.
           MOVE "E  A020ST5." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST4.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST5.
           MOVE VDUBUF TO ST-CODE(5).
     
       ENTER-ST6.
           MOVE "E  A020ST6." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST5.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST6.
           MOVE VDUBUF TO ST-CODE(6).
     
       ENTER-ST7.
           MOVE "E  A020ST7." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST6.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST7.
           MOVE VDUBUF TO ST-CODE(7).

       ENTER-ST8.
           MOVE "E  A020ST8." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST7.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST8.
           MOVE VDUBUF TO ST-CODE(8).
     
       ENTER-ST9.
           MOVE "E  A020ST9." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST8.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST9.
           MOVE VDUBUF TO ST-CODE(9).
     
       ENTER-ST10.
           MOVE "E  A020ST10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST9.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST10.
           MOVE VDUBUF TO ST-CODE(10).
     
       ENTER-ST11.
           MOVE "E  A020ST11." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST10.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST11.
           MOVE VDUBUF TO ST-CODE(11).
     
       ENTER-ST12.
           MOVE "E  A020ST12." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST11.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST12.
           MOVE VDUBUF TO ST-CODE(12).
     
       ENTER-ST13.
           MOVE "E  A020ST13." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST12.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST13.
           MOVE VDUBUF TO ST-CODE(13).
     
       ENTER-ST14.
           MOVE "E  A020ST14." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST13.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST14.
           MOVE VDUBUF TO ST-CODE(14).
     
       ENTER-ST15.
           MOVE "E  A020ST15." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST14.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST15.
           MOVE VDUBUF TO ST-CODE(15).
     
       ENTER-ST16.
           MOVE "E  A020ST16." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST15.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST16.
           MOVE VDUBUF TO ST-CODE(16).
     
       ENTER-ST17.
           MOVE "E  A020ST17." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST16.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST17.
           MOVE VDUBUF TO ST-CODE(17).
     
       ENTER-ST18.
           MOVE "E  A020ST18." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST17.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST18.
           MOVE VDUBUF TO ST-CODE(18).
     
       ENTER-ST19.
           MOVE "E  A020ST19." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST18.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST19.
           MOVE VDUBUF TO ST-CODE(19).
     
       ENTER-ST20.
           MOVE "E  A020ST20." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST19.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ST20.
           MOVE VDUBUF TO ST-CODE(20).
     
       ENTER-ORIGIN.
           MOVE "E  A040ORIGIN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-ST20.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-ORIGIN.
           MOVE VDUBUF TO ORIGIN.
     

       END-PARM.

       EXIT-PARM.
           EXIT.
      ******************************************************************
      *
      *   SETUP DATE FOR BATCH PROCESS
      *
      ******************************************************************
       EO-EXEC-AUDIT SECTION.

      ******************************************************************
      *
      *   DISPLAY REPORT PARAMETERS
      *
      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      *-----------------------------------------------------------------
       DISPLAY-PARM.
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020ST1." TO VDU.
           MOVE ST-CODE(1) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ST-CODE(2) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST3." TO VDU.
           MOVE ST-CODE(3) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST4." TO VDU.
           MOVE ST-CODE(4) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST5." TO VDU.
           MOVE ST-CODE(5) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST6." TO VDU.
           MOVE ST-CODE(6) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST7." TO VDU.
           MOVE ST-CODE(7) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST8." TO VDU.
           MOVE ST-CODE(8) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST9." TO VDU.
           MOVE ST-CODE(9) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST10." TO VDU.
           MOVE ST-CODE(10) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST11." TO VDU.
           MOVE ST-CODE(11) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST12." TO VDU.
           MOVE ST-CODE(12) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST13." TO VDU.
           MOVE ST-CODE(13) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST14." TO VDU.
           MOVE ST-CODE(14) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST15." TO VDU.
           MOVE ST-CODE(15) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST16." TO VDU.
           MOVE ST-CODE(16) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST17." TO VDU.
           MOVE ST-CODE(17) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST18." TO VDU.
           MOVE ST-CODE(18) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST19." TO VDU.
           MOVE ST-CODE(19) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST20." TO VDU.
           MOVE ST-CODE(20) TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040ORIGIN." TO VDU.
           MOVE ORIGIN TO VDUBUF.
           PERFORM SCREENIO.
      ******************************************************************
      *
      *    P R O C E S S - L N - F I L E
      *               
      *       - MOVE 0 TO LN-MTD-PRIN, LC, INT, OT, EX. 
      *
      ******************************************************************
       PROCESS-LN-FILE SECTION.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

      *-----------------------------------------------------------------
       PROCESS-LN-FILE-NEXT.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-LN-FILE-EXIT.

           IF PL = "N"
              IF LN-PLDATE NOT = 0
                 GO TO PROCESS-LN-FILE-NEXT.

           IF ZBAL = "N"
              IF LN-CURBAL = 0
                 GO TO PROCESS-LN-FILE-NEXT.


           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO PROCESS-LN-FILE-NEXT.

           MOVE "N" TO FOUND-MATCH.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF BW-STATE = ST-CODE(SUB)
                 MOVE "Y" TO FOUND-MATCH
              END-IF
           END-PERFORM.

           IF FOUND-MATCH NOT = "Y"
              GO TO PROCESS-LN-FILE-NEXT.
            
           MOVE ORIGIN TO LN-ORIGIN.

           PERFORM REWRITE-LN1-FILE.

           GO TO PROCESS-LN-FILE-NEXT.

      *-----------------------------------------------------------------
       PROCESS-LN-FILE-EXIT.
           EXIT.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO ORIGIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ORIGIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ORIGIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE ORIGIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ORIGIN-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/ORIGIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     M I S C E L L A N E O U S      P A R A G R A P H S  
      *
      ******************************************************************
       MISC-PARA SECTION.
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *    I O    R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D    R O U T I N E 
      *
      ******************************************************************
       END-PROGRAM SECTION.

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ORIGIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
