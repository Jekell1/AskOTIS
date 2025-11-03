       IDENTIFICATION DIVISION.
       PROGRAM-ID.      MAKERS.
       DATE-WRITTEN.    122391.
      ********************************************************************
      *          WINDOW MAKERS DISPLAY
      *               CONTROL "N" FROM INQUIRY AND PAYMENT POSTING
      *
      * REV:
      *  BLV 022697 IF NO COLLATERAL, STILL DISPLAYING COLLATERAL FROM
      *             LAST BORROWER CALLED UP
      *  JTG 052797 CHANGED TO DISPLAY COLLATERAL FROM RECORD 3
      *  GLF 100997 CHANGED TO DISPLAY BW-OTHPHONE
      *  JTG 021698 CHANGED TO ALLOW CTRL-N TO PHONE NO'S WINDOW PHONES.C
      *  GLF 021998 CHANGED LEGEND TO DISPLAY OTHER CTRL N OPTION
      *  GLF 030298 REMOVED BW-OTHPONE AND ARREANGED FIELDS ACCORDINGLY
      *  MJD 000502 ADDED BMFILE. IF CONTACT EXIST THEN CALL WI/PHONE2
      *             OTHERWISE CALL WI/PHONES WHEN ^N IS HIT. (TFC PR#37)
      *  GLB 030429 DISPLAY BOTH HHG COLLATERAL GOODS AND BOTH CARS,
      *             LENDMARK PR# 2062
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 20220803 HARD CODED START
      *  BAH 20250325 BORROWER CONTACT PHONES ARE IN BW-REC IN S35, NOT
      *               IN MEMOS, REMOVE BM   S35Q-192
      ********************************************************************
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
           "@(#)WI/MAKERS.CBL S35 02/22/24-12:50:52 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".


       01  ERRCD                 PIC X.
       01  SUB                   PIC 9(3) COMP-3  VALUE 0.
       01  MAKERSUB              PIC 9(3) COMP-3  VALUE 0.
       01  NO-MAKERS             PIC 9.

      **************************************************
      *    DISPLAY BUFFER
      **************************************************
       01  DISPLAY-BUFFER     VALUE " ".
           03  D-BUF          OCCURS 2.
               05  F01P        PIC X(6).
               05  F01         PIC X(30).
               05  F02         PIC 999/999/9999    BLANK ZERO.
               05  F03         PIC 999/999/9999    BLANK ZERO.
               05  F031        PIC ZZZ9            BLANK ZERO.
               05  F04         PIC X(24).
               05  F05         PIC X(24).
               05  F06         PIC Z9              BLANK ZERO.
               05  F061        PIC ZZZ,ZZ9.99      BLANK ZERO.
               05  F07         PIC X(9).
               05  F071P       PIC X(9).
               05  F071        PIC ZZZ,ZZ9.99      BLANK ZERO.
           03  FCOLLIDP        PIC X(6).
           03  FCOLLID         PIC X(69).
           03  FCOLLIDP2       PIC X(6).
           03  FCOLLID2        PIC X(69).
           03  FCOLLIDP3       PIC X(6).
           03  FCOLLID3        PIC X(69).
           03  FCOLLIDP4       PIC X(6).
           03  FCOLLID4        PIC X(69).
       01  DISPLAY-LIST.
           03  FILLER      PIC X(54) VALUE
           "010P:S 010 020 030 031 040 050 060 061 070 071P:S 071 ".
           03  FILLER      PIC X(54) VALUE
           "150P:S 150 160 170 171 180 190 200 201 210 211P:S 211 ".
           03  FILLER      PIC X(36) VALUE
           "COLLIDP:S COLLID COLLIDP2:S COLLID2 ".
           03  FILLER      PIC X(38) VALUE
           "COLLIDP3:S COLLID3 COLLIDP4:S COLLID4.".

       01  LEGEND                PIC X(70).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".

       01  MAKERS-WINDOW-HANDLE   PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/MAKERS_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBWI/MAKERS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/MAKERS_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-CLMENU-SELECTION.
           MOVE "MAKERS" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 08 * VDU-WIN-L-OFFSET
               AT SCREEN COL 01 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 80 + VDU-WIN-W-MOD
               LINES 14 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS MAKERS-WINDOW-HANDLE.

            PERFORM FORM-RESET.

       MAIN-MODULE.
           MOVE " " TO ERRCD.
           PERFORM DISPLAY-TITLE.

           MOVE SPACES TO FCOLLIDP   FCOLLID
                          FCOLLIDP2  FCOLLID2
                          FCOLLIDP3  FCOLLID3
                          FCOLLIDP4  FCOLLID4.

           PERFORM GET-LN1.

           PERFORM VARYING MAKERSUB FROM 1 BY 1 UNTIL MAKERSUB > 2
              PERFORM SETUP-MAKER
           END-PERFORM.

      *  ORIGINAL COLLATERAL FROM LOAN AND TRAILER
           MOVE "HHG 1:" TO FCOLLIDP.
           PERFORM GET-LT.
           IF IO-FG = 0
              MOVE SPACES TO VDU
              IF LN-COLLID = SPACES
                 MOVE LTC-HHGS(1) TO FCOLLID
              ELSE
                 STRING LN-COLLID " " LTC-HHGS(1)
                           DELIMITED BY SIZE INTO VDU
                 MOVE VDU TO FCOLLID
           ELSE
              MOVE LN-COLLID TO FCOLLID.

      *  2ND HHG COLLATERAL FROM TRAILER
           MOVE "HHG 2:" TO FCOLLIDP2.
           IF LTC-HHGS(2) NOT = SPACES
              MOVE LTC-HHGS(2) TO FCOLLID2.

      *  1ST CAR COLLATERAL TRAILER
           MOVE "CAR 1:" TO FCOLLIDP3.
           IF LTC-MAKE(1) NOT = SPACES OR LTC-MODEL(1) NOT = SPACES OR
               LTC-VIN(1) NOT = SPACES
              MOVE SPACES TO VDU
              STRING LTC-YEAR(1) " " LTC-MAKE(1) " " LTC-MODEL(1) " "
                 LTC-STYLE(1) " " LTC-CLASS(1) " VIN: " LTC-VIN(1) " "
                 DELIMITED BY "  " INTO VDU
              MOVE VDU TO FCOLLID3.

      *  2ND CAR COLLATERAL TRAILER
           MOVE "CAR 2:" TO FCOLLIDP4.
           IF LTC-MAKE(2) NOT = SPACES OR LTC-MODEL(2) NOT = SPACES OR
               LTC-VIN(2) NOT = SPACES
              MOVE SPACES TO VDU
              STRING LTC-YEAR(2) " " LTC-MAKE(2) " " LTC-MODEL(2) " "
                 LTC-STYLE(2) " " LTC-CLASS(2) " VIN: " LTC-VIN(2) " "
                 DELIMITED BY "  " INTO VDU
              MOVE VDU TO FCOLLID4.

           PERFORM DISPLAY-ALL-LINES.

      *    PERFORM GET-LN1.

       ACCEPT-IT.
           IF NO-MAKERS < 3
                 MOVE " CTRL N MISC PHONE, EXIT-ANYKEY" TO LEGEND
              ELSE
                 MOVE " CTRL N PH#, EXIT-ANYKEY, ^-TOGGLE" TO LEGEND.

           PERFORM SEND-LEGEND.

           

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF NO-MAKERS > 2
              IF VDUSTATUS = "1U" OR "1D"
                 IF MAKERSUB = 3
                    MOVE 3 TO MAKERSUB
                 ELSE
                    MOVE 1 TO MAKERSUB
                 END-IF
              PERFORM 2 TIMES
                 PERFORM SETUP-MAKER
                 ADD 1 TO MAKERSUB
              END-PERFORM
              PERFORM DISPLAY-ALL-LINES
              GO TO ACCEPT-IT.
           GO TO END-ROUTINE.

      *******************************************
       SETUP-MAKER SECTION.
      *******************************************

           MOVE MAKERSUB TO SUB.
           IF MAKERSUB > 2
              SUBTRACT 2 FROM SUB.
           MOVE " " TO D-BUF(SUB).

           IF EXT-WI-CL-SSNO(MAKERSUB) = 0
              GO TO SETUP-MAKER-EXIT.

           MOVE EXT-WI-CL-SSNO(MAKERSUB) TO BW-SSNO.
           PERFORM GET-BW.
           IF IO-FG = 9
              GO TO SETUP-MAKER-EXIT.

           IF BW-CONTACT-PHONE(1) NOT = 0
              MOVE "WI/PHONE2" TO VDU-CTRL-N-PROG
           ELSE
              MOVE "WI/PHONES" TO VDU-CTRL-N-PROG.

           IF MAKERSUB = 1
              MOVE "MAKER:" TO F01P(SUB)
           ELSE
              IF EXT-WI-CL-MAKERCD(MAKERSUB) = "J"
                 MOVE "JOINT:" TO F01P(SUB)
           ELSE
              IF EXT-WI-CL-MAKERCD(MAKERSUB) = "E"
                 MOVE "ENDOR:" TO F01P(SUB).

           MOVE SPACES TO F01(SUB).
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO F01(SUB).
           MOVE BW-PHONE TO F02(SUB).
           INSPECT F02(SUB) REPLACING ALL "/" BY "-".
           MOVE BW-EMPPHONE TO F03(SUB).
           INSPECT F03(SUB) REPLACING ALL "/" BY "-".
           MOVE BW-EMPEXT TO F031(SUB).
           MOVE BW-EMPLOYER TO F04(SUB).
           MOVE BW-EMPLPOS TO F05(SUB).
           MOVE BW-EMPYEARS TO F06(SUB).
           MOVE BW-EMPPAY TO F061(SUB).
           MOVE BW-WORKHRS TO F07(SUB).
           MOVE BW-EMPPAYSR TO F071P(SUB).
           MOVE BW-EMPPAYOT TO F071(SUB).

       SETUP-MAKER-EXIT.
           EXIT.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPACES UPON MAKERS-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY MAKERS-SCREEN UPON MAKERS-WINDOW-HANDLE.
           MOVE MAKERS-FORM-FIELDS TO WR-BUF.
           MOVE MAKERS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE MAKERS-HELP--FILE TO HELP-LINK-FILE.
           MOVE MAKERS-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/MAKERS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-TITLE.
           MOVE 0 TO NO-MAKERS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               IF EXT-WI-CL-SSNO(SUB) NOT = 0
                  ADD 1 TO NO-MAKERS
               END-IF
           END-PERFORM.
           MOVE "S  A110TITLE." TO VDU.
           STRING " MAKERS: " NO-MAKERS "      "
              DELIMITED BY SIZE INTO VDUBUF.
           PERFORM SCREENIO.
       GET-LT.
           MOVE EXT-WI-CL-OWNBR  TO LTC-BRNO.
           MOVE EXT-WI-CL-ACCTNO TO LTC-ACCTNO.
           MOVE 1                TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.
       GET-LN1.
           MOVE EXT-WI-CL-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           IF IO-FG = 9
              MOVE "**MISSING**" TO BW-LNAME.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
       DISPLAY-ALL-LINES.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUFFER TO WR-BUF.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY MAKERS-SCREEN.
           DESTROY MAKERS-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
