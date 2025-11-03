       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRBPCP.
       DATE-WRITTEN.    041406.
      *********************************************************
      *                 (SP)
      *  DESCRIPTION:   COPY BRANCH BP FIELDS
      *
      * REV:
      *  150930 BAH CHANGE BRANCH TO INDEXED, PD0005
      * KEC 2018-1004 CONVERTED BRFILE FOR A32 & A34
      * BAH 20191004 REMOVED ACCESS-CALL SOURCE & TARGET
      * BAH 20230210 ADD A "TO" BRANCH RANGE #1590
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BRBPCP.CBL S35 02/23/24-10:26:03 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/GB01BR.CPY" REPLACING LEADING "BR" BY "BROR".
       01  NEWBR1                    PIC 9(4).
       01  NEWBR2                    PIC 9(4).


       01  MESS-LIST       PIC X(7) VALUE "MESS:S.".

       01  ERRCD           PIC X    VALUE " ".

       01  IO-TYPE         PIC X(3).


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  BRBPCP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BRBPCP_DEF.CPY".
       COPY "LIBSP/BRBPCP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BRBPCP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BRBPCP"      TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BRBPCP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-BR-FILE.

       ENTER-KEYS.
      D    STOP MESS.
           PERFORM UPDATE-FORM.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-BRANCHES.

           GO TO ENTER-KEYS.

      ************************************
       UPDATE-FORM SECTION.
      ************************************
       ENTER-BRANCH.
           MOVE SPACES TO IO-TYPE.

           MOVE "S  A400DISP1." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN040KEY1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BR-NO.

           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

      * SAVE ORIGINAL BRANCH RECORD
           MOVE BR-REC TO BROR-REC.

           MOVE "S  A400DISP1." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-TO-BRANCH1.
           MOVE "S  A400DISP2." TO VDU.
           PERFORM SCREENIO.

           MOVE "ENNN040BEG-KEY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-UPDATE-FORM.
           IF VDUSTATUS = "16"
              GO TO ALL-TO-BRANCHES.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TO-BRANCH1.

           MOVE VDUNUM0 TO NEWBR1.

           IF NEWBR1 = BR-NO
              MOVE "CANT BE SAME BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-BRANCH1.

           IF NEWBR1 = 0
              GO TO ENTER-TO-BRANCH1.

           MOVE NEWBR1 TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TO-BRANCH1.

           MOVE "S  A400DISP2." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-TO-BRANCH2.
           MOVE "ENNN040END-KEY2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-TO-BRANCH1.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TO-BRANCH2.

           MOVE VDUNUM0 TO NEWBR2.

           IF NEWBR2 < NEWBR1
              GO TO ENTER-TO-BRANCH2.

           GO TO ENTER-ACCEPT-YN.

       ALL-TO-BRANCHES.
           MOVE "SN N040BEG-KEY2." TO VDU.
           MOVE 0                  TO VDUNUM0 NEWBR1.
           PERFORM SCREENIO.

           MOVE "S  A400DISP2." TO VDU.
           MOVE "*** ALL BRANCHES ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N040END-KEY2." TO VDU.
           MOVE 9999             TO VDUNUM0 NEWBR2.
           PERFORM SCREENIO.
       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TO-BRANCH1.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-UPDATE-FORM.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ACCEPT-YN.
           IF VDUBUF = "N"
              GO TO ENTER-BRANCH
           ELSE
              IF VDUBUF NOT = "Y"
              GO TO ENTER-ACCEPT-YN.

       EXIT-UPDATE-FORM.
           EXIT.

      ******************************************
       UPDATE-BRANCHES SECTION.
      ******************************************
           MOVE NEWBR1 TO BR-NO.
           PERFORM START-BR-FILE.
       UPDATE-NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-UPDATE-BRANCHES.

           IF BR-NO > NEWBR2
              GO TO EXIT-UPDATE-BRANCHES.

           IF BR-NO = BROR-NO
              GO TO UPDATE-NEXT-BRANCH.

           MOVE BROR-BP-NEXT-BATCH   TO BR-BP-NEXT-BATCH.
           MOVE BROR-BP-BRANCH       TO BR-BP-BRANCH.
           MOVE BROR-BP-NOPAYOFF     TO BR-BP-NOPAYOFF.
           MOVE BROR-LBOX-NOPAYOFF   TO BR-LBOX-NOPAYOFF.
           MOVE BROR-BP-ODDPAY       TO BR-BP-ODDPAY.
           MOVE BROR-LBOX-ODDPAY     TO BR-LBOX-ODDPAY.
           MOVE BROR-BP-LC           TO BR-BP-LC.
           MOVE BROR-LBOX-LC         TO BR-LBOX-LC.
           MOVE BROR-BP-NOPRIN       TO BR-BP-NOPRIN.
           MOVE BROR-LBOX-NOPRIN     TO BR-LBOX-NOPRIN.
           MOVE BROR-BP-FLASH        TO BR-BP-FLASH.
           MOVE BROR-LBOX-FLASH      TO BR-LBOX-FLASH.
           MOVE BROR-BP-ACTIONCD     TO BR-BP-ACTIONCD.
           MOVE BROR-LBOX-ACTIONCD   TO BR-LBOX-ACTIONCD.
           MOVE BROR-BP-PL           TO BR-BP-PL.
           MOVE BROR-LBOX-PL         TO BR-LBOX-PL.
           MOVE BROR-BP-JD           TO BR-BP-JD.
           MOVE BROR-LBOX-JD         TO BR-LBOX-JD.
           MOVE BROR-BP-ALLOW-PMT-ACCT-OTHBAL
                                     TO BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BROR-LBOX-ALLOW-PMT-ACCT-OTHBAL
                                     TO BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BROR-BP-MAXCON       TO BR-BP-MAXCON.
           MOVE BROR-LBOX-MAXCON     TO BR-LBOX-MAXCON.
           MOVE BROR-BP-MAXREC       TO BR-BP-MAXREC.
           MOVE BROR-LBOX-MAXREC     TO BR-LBOX-MAXREC.
           MOVE BROR-BP-PAYOFF-NONCASH TO BR-BP-PAYOFF-NONCASH.
           MOVE BROR-LBOX-PAYOFF-NONCASH 
                                     TO BR-LBOX-PAYOFF-NONCASH.
           MOVE BROR-BP-PAYOFF-OV    TO BR-BP-PAYOFF-OV.
           MOVE BROR-LBOX-PAYOFF-OV  TO BR-LBOX-PAYOFF-OV.

           PERFORM REWRITE-BR-FILE.

           GO TO UPDATE-NEXT-BRANCH.

       EXIT-UPDATE-BRANCHES.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO BRBPCP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRBPCP-SYSDATE.
           DISPLAY SPACES UPON BRBPCP-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY BRBPCP-SCREEN UPON BRBPCP-WINDOW-HANDLE.
           MOVE BRBPCP-FORM-FIELDS TO WR-BUF.
           MOVE BRBPCP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE BRBPCP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRBPCP-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BRBPCP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************
       MISC-PARAGRAPHS SECTION.
      **********************************
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       COPY-BEBRN.
           MOVE "COPY IN PROGRESS................" TO MESS.
           PERFORM SEND-LEGEND.
       COPY-END.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.


      ***************************************
       IO-ROUTINE SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************************
       END-ROUTINE SECTION.
      ***************************************
       ENDIT.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GB/INSTMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BRBPCP-WINDOW-HANDLE.
           EXIT PROGRAM.
