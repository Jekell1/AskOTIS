       IDENTIFICATION DIVISION.
       PROGRAM-ID.      REALPY.
       DATE-WRITTEN.    020801.
      ********************************************************************
      *  DESC:  REAL ESTATE 'PY' PAYMENT INPUT/DISBURSEMENT
      *         WINDOW CALLED FROM LP/LONPFC.C
      *
      * REV:
      *  JTG 010208 CREATE NEW FROM  RVPEAP.C LENDMARK  PR#1045 &  PR#1380
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *  BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/REALPY.CBL A30 05/16/16-13:54:38 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  REALPY-ONE                   PIC S9.
       01  SUBX                         PIC 9.
       01  ALLOW-F5-SET-EXTRA-FG        PIC X.

       01  DISPLAY-NUM                  PIC Z(6)9.99-.
       01  REALPY-TRAPP                 PIC S9(7)V99 COMP.
       01  REALPY-TRPROOF               PIC S9(7)V99 COMP.

       01  REALPY-DISPLAY-LIST.
           03  FILLER                   PIC X(39) VALUE
           "010 020 030 040 050 060 070 080 TOTPAY ".
           03  FILLER                   PIC X(35) VALUE
           "090 100 110 120 130 140 150 TOTAPP ".
           03  FILLER                   PIC X(39) VALUE
           "PDTH LCPDTH OTHBAL LCBAL INTBAL CURBAL ".
           03  FILLER                   PIC X(15) VALUE
           "INTLFP PRINLFP.".

       01  REALPY-DISPLAY-BUF.
           03  D-PAYDATE        PIC 99/99/99     BLANK ZERO.
           03  D-TRCD           PIC X(2).
           03  D-REFNO          PIC X(5).
           03  D-TRAMT          PIC Z(6)9.99-    BLANK ZERO.

           03  D-POST-REGPY     PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-EXTRA-PRIN PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-OTH       PIC Z(6)9.99-    BLANK ZERO.
           03  D-POST-LCHG      PIC Z(6)9.99-    BLANK ZERO.
           03  D-TOTPMT         PIC Z(6)9.99-    BLANK ZERO.

           03  D-PDUE-APINT     PIC Z(6)9.99-    BLANK ZERO.
           03  D-PDUE-APPRIN    PIC Z(6)9.99-    BLANK ZERO.
           03  D-CUR-APINT      PIC Z(6)9.99-    BLANK ZERO.
           03  D-CUR-APPRIN     PIC Z(6)9.99-    BLANK ZERO.
           03  D-AP-EXTRA-PRIN    PIC Z(6)9.99-    BLANK ZERO.
           03  D-APOTH          PIC Z(6)9.99-    BLANK ZERO.
           03  D-APLC           PIC Z(6)9.99-    BLANK ZERO.
           03  D-TOTAPP         PIC Z(6)9.99-    BLANK ZERO.

           03  D-PDTH           PIC 99/99/99.
           03  D-LCPDTH         PIC 99/99/99.
           03  D-OTHBAL         PIC Z(6)9.99-    BLANK ZERO.
           03  D-LCBAL          PIC Z(6)9.99-    BLANK ZERO.
           03  D-INTBAL         PIC Z(6)9.99-    BLANK ZERO.
           03  D-CURBAL         PIC Z(6)9.99-    BLANK ZERO.

           03  D-INT-SINCE-LFP  PIC Z(6)9.99-    BLANK ZERO.
           03  D-PRIN-SINCE-LFP PIC Z(6)9.99-    BLANK ZERO.


      *************************************************************************

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LFPAPW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".

       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/AFPDLLPW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".

       01 REALPY-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/REALPY_DEF.CPY".
       COPY "LIBWI/REALPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/REALPYW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LP.CPY".

       SCREEN SECTION.
       COPY "LIBWI/REALPY_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME REALPYW-FIELDS SP-REC LN-REC LP-REC
                  EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "REALPY" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 03 * VDU-WIN-L-OFFSET
               AT SCREEN COL 02 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 78 + VDU-WIN-W-MOD
               LINES 20 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS REALPY-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM REALPY-WIN.

           GO TO END-ROUTINE.

      **************************************************************
       REALPY-WIN SECTION.
      **************************************************************

      *-----------------------------------------------------
      *    DISPLAY REQUESTED DISPLAY PER: SUBX
      *        1 -  PAYMENT CALCULATED DEFAULT
      *        2 -  ENTERED LFP PAYMENT DISTRIBUTION
      *-----------------------------------------------------
       REALPY-WIN-RESET-DISPLAY.
           MOVE REALPYW-TABLE(1) TO REALPYW-TABLE(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.

           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-POST.
           IF ( REALPYW-POST-EXTRA-PRIN(1) = 0 ) OR
              ( REALPYW-OTHBAL(1) = 0 AND REALPYW-LCBAL(1) = 0 )
              GO TO REALPY-WIN-ACCEPT.

           IF REALPYW-OTHBAL(1) NOT = 0
              GO TO REALPY-WIN-POST-OTH.
           IF REALPYW-LCBAL(1) NOT = 0
              GO TO REALPY-WIN-POST-LCHG.
           GO TO REALPY-WIN-ACCEPT.

      *---------------------------------------------------------------------
       REALPY-WIN-POST-REGPY.
           GO TO REALPY-WIN-POST-EXTRAPRIN.

           MOVE "F1, F4, F7, ENTER AMOUNT TO POST REGULAR PAYMENT:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072050." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-POST-REGPY.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-POST-REGPY.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-POST-REGPY.

           MOVE VDUNUM2 TO REALPYW-POST-REGPY(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-POST-EXTRAPRIN.
           IF ( REALPYW-POST-EXTRA-PRIN(1) = 0  ) OR
              ( REALPYW-OTHBAL(1) = 0 AND REALPYW-LCBAL(1) = 0 )
              GO TO REALPY-WIN-ACCEPT.

           IF REALPYW-OTHBAL(1) NOT = 0
              GO TO REALPY-WIN-POST-OTH.
           IF REALPYW-LCBAL(1) NOT = 0
              GO TO REALPY-WIN-POST-LCHG.
           GO TO REALPY-WIN-ACCEPT.

           MOVE
           "F1, F4, F7, ENTER AMOUNT TO POST TO EXTRA PRINCIPAL:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072060." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-POST-EXTRAPRIN.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-POST-REGPY.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-POST-EXTRAPRIN.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-POST-EXTRAPRIN.

           MOVE VDUNUM2 TO REALPYW-POST-EXTRA-PRIN(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
      *---------------------------------------------------------------------
       REALPY-WIN-POST-OTH.

           MOVE
           "F1, F4, F7-DONE, ENTER AMOUNT TO POST TO OTHER CHARGES:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072070." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-POST-OTH.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-POST.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-POST-OTH.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-POST-OTH.
           IF VDUNUM2 > (REALPYW-OTHBAL(1) + REALPYW-APOTH(1))
              COMPUTE DISPLAY-NUM = REALPYW-OTHBAL(1) + REALPYW-APOTH(1)
              MOVE " " TO MESS
              STRING "EXCEEDS LOAN'S OTHER BALANCE OF "
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-OTH.
           IF VDUNUM2 > (REALPYW-POST-EXTRA-PRIN(1)
                          + REALPYW-POST-OTH(1)
                          + REALPYW-POST-LCHG(1)
                        )
              COMPUTE DISPLAY-NUM = REALPYW-POST-EXTRA-PRIN(1)
                                  + REALPYW-POST-OTH(1)
                                  + REALPYW-POST-LCHG(1)
              MOVE " " TO MESS
              STRING "EXCEEDS CALCULATED EXTRA PRINCIPAL OF"
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-OTH.
           IF (VDUNUM2 - REALPYW-APOTH(2)) > REALPYW-AP-EXTRA-PRIN(2)
              COMPUTE DISPLAY-NUM = REALPYW-AP-EXTRA-PRIN(2)
              STRING "WILL EXCEED REMAINING EXTRA PRINCIPAL OF "
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-OTH.

           SUBTRACT REALPYW-POST-OTH(2) FROM REALPYW-APOTH(2)
                                            REALPYW-CURBAL(2).
           ADD REALPYW-POST-OTH(2) TO REALPYW-OTHBAL(2)
                                     REALPYW-AP-EXTRA-PRIN(2)
                                     REALPYW-POST-EXTRA-PRIN(2).
           MOVE VDUNUM2 TO REALPYW-POST-OTH(2).
           ADD REALPYW-POST-OTH(2) TO REALPYW-APOTH(2)
                                     REALPYW-CURBAL(2).
           SUBTRACT REALPYW-POST-OTH(2) FROM REALPYW-OTHBAL(2)
                                            REALPYW-AP-EXTRA-PRIN(2)
                                            REALPYW-POST-EXTRA-PRIN(2).

           PERFORM REMAIN-INT-PRIN-SINCE-LFP.
           MOVE LFPAP-REMAIN-INT-SINCE-LFP
                                          TO REALPYW-INT-SINCE-LFP(2).
           MOVE LFPAP-REMAIN-PRIN-SINCE-LFP
                                          TO REALPYW-PRIN-SINCE-LFP(2).

           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-POST-LCHG.

           MOVE "F1, F4, F7, ENTER AMOUNT TO POST TO LATE CHARGES:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072080." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-POST-LCHG.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-POST-OTH.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-POST-LCHG.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-POST-LCHG.
           IF VDUNUM2 > (REALPYW-LCBAL(1) + REALPYW-APLC(1))
              COMPUTE DISPLAY-NUM = REALPYW-LCBAL(1) + REALPYW-APLC(1)
              MOVE " " TO MESS
              STRING "EXCEEDS LOAN'S LATE CHARGE BALANCE OF "
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-LCHG.
           IF VDUNUM2 > (REALPYW-POST-EXTRA-PRIN(1)
                          + REALPYW-POST-OTH(1)
                          + REALPYW-POST-LCHG(1)
                        )
              COMPUTE DISPLAY-NUM = REALPYW-POST-EXTRA-PRIN(1)
                                  + REALPYW-POST-OTH(1)
                                  + REALPYW-POST-LCHG(1)
              MOVE " " TO MESS
              STRING "EXCEEDS CALCULATED EXTRA PRINCIPAL OF"
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-LCHG.
           IF (VDUNUM2 - REALPYW-APLC(2)) > REALPYW-AP-EXTRA-PRIN(2)
              COMPUTE DISPLAY-NUM = REALPYW-AP-EXTRA-PRIN(2)
              MOVE " " TO MESS
              STRING "WILL EXCEED REMAINING EXTRA PRINCIPAL OF"
                     DISPLAY-NUM
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO REALPY-WIN-POST-LCHG.

           SUBTRACT REALPYW-POST-LCHG(2) FROM REALPYW-APLC(2)
                                             REALPYW-CURBAL(2).
           ADD REALPYW-POST-LCHG(2) TO REALPYW-LCBAL(2)
                                      REALPYW-AP-EXTRA-PRIN(2)
                                      REALPYW-POST-EXTRA-PRIN(2).
           MOVE VDUNUM2 TO REALPYW-POST-LCHG(2).
           ADD REALPYW-POST-LCHG(2) TO REALPYW-APLC(2)
                                      REALPYW-CURBAL(2).
           SUBTRACT REALPYW-POST-LCHG(2) FROM REALPYW-LCBAL(2)
                                             REALPYW-AP-EXTRA-PRIN(2)
                                             REALPYW-POST-EXTRA-PRIN(2).

           PERFORM REMAIN-INT-PRIN-SINCE-LFP.
           MOVE LFPAP-REMAIN-INT-SINCE-LFP
                                           TO REALPYW-INT-SINCE-LFP(2).
           MOVE LFPAP-REMAIN-PRIN-SINCE-LFP
                                           TO REALPYW-PRIN-SINCE-LFP(2).

           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

           GO TO REALPY-WIN-ACCEPT.

      *---------------------------------------------------------------------
       REALPY-WIN-PDUE-APINT.
      *    IF REALPYW-RCURLC(1) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-PDUE-APPRIN
      *       ELSE
      *          GO TO REALPY-WIN-POST-LCHG.

           MOVE
           "F1, F4, F7, ENTER AMOUNT TO APPLY TO PASTDUE INTEREST:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072090." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-PDUE-APINT.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-POST-LCHG.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-PDUE-APINT.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-PDUE-APINT.

           MOVE VDUNUM2 TO REALPYW-PDUE-APINT(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-PDUE-APPRIN.
      *    IF REALPYW-RCURINT(1) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-CUR-APINT
      *       ELSE
      *          GO TO REALPY-WIN-PDUE-APINT.

           MOVE
           "F1, F4, F7-DONE, ENTER AMOUNT TO APPLY TO PASTDUE PRIN:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072100." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-PDUE-APPRIN.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-PDUE-APINT.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-PDUE-APPRIN.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-PDUE-APPRIN.

           MOVE VDUNUM2 TO REALPYW-PDUE-APPRIN(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-CUR-APINT.
      *    IF REALPYW-RCURPRIN(1) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-CUR-APPRIN
      *       ELSE
      *          GO TO REALPY-WIN-PDUE-APPRIN.

           MOVE
           "F1, F4, F7, ENTER AMOUNT TO APPLY TO CURRENT INTEREST:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072110." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-CUR-APINT.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-PDUE-APPRIN.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-CUR-APINT.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-CUR-APINT.

           MOVE VDUNUM2 TO REALPYW-PDUE-APINT(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-CUR-APPRIN.
      *    IF ( REALPY-RTOTOTH(1) - REALPY-RPDUEOTH(1)
      *                           - REALPY-RCUROTH(1)
      *       ) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-APOTH
      *       ELSE
      *          GO TO REALPY-WIN-CUR-APINT.

           MOVE "F1, F4, F7-DONE, ENTER AMOUNT TO APPLY TO CUR PRIN:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072120." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-CUR-APPRIN.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-CUR-APINT.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-CUR-APPRIN.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-CUR-APPRIN.

           ADD VDUNUM2 TO REALPYW-CUR-APPRIN(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-EXTRAPRIN.
      *    IF ( REALPY-RTOTLC(1) - REALPY-RPDUELC(1)
      *                           - REALPY-RCURLC(1)
      *       ) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-APLC
      *       ELSE
      *          GO TO REALPY-WIN-CUR-APPRIN.

           MOVE
           "F1, F4, F7, ENTER AMOUNT TO APPLY TO EXTRA PRINCIPAL:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072130." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-EXTRAPRIN.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-CUR-APPRIN.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-EXTRAPRIN.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-EXTRAPRIN.

           ADD VDUNUM2 TO REALPYW-AP-EXTRA-PRIN(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-APOTH.
      *    IF ( REALPY-RTOTLC(1) - REALPY-RPDUELC(1)
      *                           - REALPY-RCURLC(1)
      *       ) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-APLC
      *       ELSE
      *          GO TO REALPY-WIN-CUR-APPRIN.

           MOVE
           "F1, F4, F7, ENTER AMOUNT TO APPLY TO OTHER CHARGES:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072140." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-APOTH.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-CUR-APPRIN.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-APOTH.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-APOTH.

           ADD VDUNUM2 TO REALPYW-APOTH(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.

      *---------------------------------------------------------------------
       REALPY-WIN-APLC.
      *    IF ( REALPY-RTOTINT(1) - REALPY-RPDUEINT(1)
      *                           - REALPY-RCURINT(1)
      *       ) = 0
      *       IF REALPY-ONE = +1
      *          GO TO REALPY-WIN-EXTRAPRIN
      *       ELSE
      *          GO TO REALPY-WIN-APOTH.

           MOVE
           "F1, F4, F7-DONE, ENTER AMOUNT TO APPLY TO LATE CHARGES:"
                                                               TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS072150." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF VDUSTATUS = "18"
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-APLC.
           IF VDUSTATUS = "1U"
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-APOTH.
           IF VDUSTATUS = "1>"
              GO TO REALPY-WIN-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO REALPY-WIN-APLC.
           IF VDUNUM2 < 0
              GO TO REALPY-WIN-APLC.

           ADD VDUNUM2 TO REALPYW-APLC(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.
           MOVE +1 TO REALPY-ONE.
      *---------------------------------------------------------------------
       REALPY-WIN-ACCEPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DETERMINE IF MONIES IN APPLIED TO REGULAR PAYMENT
      *    CAN BE SWITCHED TO EXTRA PRINCIPAL

           MOVE "N" TO ALLOW-F5-SET-EXTRA-FG.
           IF REALPYW-POST-EXTRA-PRIN(2) = 0
              MOVE REALPYW-PAYDATE TO NUM-DATE
              MOVE LP-PDTH-DATE    TO SYS-DATE
              MOVE 01              TO NUM-DA
                                      S-DD
              IF NUM-DATE = SYS-DATE
                 MOVE "Y" TO ALLOW-F5-SET-EXTRA-FG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ALLOW-F5-SET-EXTRA-FG = "Y"
              IF ( REALPYW-POST-EXTRA-PRIN(1) = 0 ) OR
                 ( REALPYW-OTHBAL(1) = 0 AND REALPYW-LCBAL(1) = 0 )
                 MOVE " F1-RESET, F4, (EXTRA)F5, F7-ACCEPT" TO MESS
              ELSE
                 STRING " F1-RESET, F4, (EXTRA)F5, ^-CHANGE "
                        "DISTRIBUTION, F7-ACCEPT:"
                        DELIMITED BY SIZE INTO MESS
              END-IF
           ELSE
              IF ( REALPYW-POST-EXTRA-PRIN(1) = 0 ) OR
                 ( REALPYW-OTHBAL(1) = 0 AND REALPYW-LCBAL(1) = 0 )
                 MOVE " F1-RESET, F4, F7-ACCEPT:" TO MESS
              ELSE
                 MOVE
                 " F1-RESET, F4, ^-CHANGE DISTRIBUTION, F7-ACCEPT:"
                                                           TO MESS.
           PERFORM SEND-WSEL.
           MOVE "E RA010WSELX." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              PERFORM ALARM
              GO TO REALPY-WIN-RESET-DISPLAY.
           IF F4-KEY
              PERFORM REALPY-WIN-ORIG-APPLIED
              GO TO REALPY-WIN-ACCEPT.
           IF F5-KEY
              IF ALLOW-F5-SET-EXTRA-FG = "Y"
                 IF VDUBUF = "EXTRA"
                    PERFORM REALPY-WIN-SET-EXTRA-PRIN
                    MOVE REALPYW-TABLE(2) TO REALPYW-TABLE(1)
                    MOVE -1 TO REALPY-ONE
                    GO TO REALPY-WIN-POST.
           IF UP-KEY
              MOVE -1 TO REALPY-ONE
              GO TO REALPY-WIN-POST.
           IF NOT F7-KEY
              GO TO REALPY-WIN-ACCEPT.

      *---------------------------------------------------------------------
      *    F7 DEPRESSED
      *---------------------------------------------------------------------
       REALPY-WIN-TEST-DONE.
           PERFORM REALPY-GET-TRPROOF.
           IF REALPY-TRPROOF NOT = 0
              IF REALPY-TRPROOF > 0
                 MOVE "THE PAYMENT IS OVER APPLIED!" TO MESS
              ELSE
                 MOVE "THE PAYMENT IS NOT FULLY APPLIED!" TO MESS
              END-IF
              PERFORM SEND-MESS
              GO TO REALPY-WIN-ACCEPT.

       REALPY-WIN-EXIT.

      *************************************************************
       REALPY-WIN-SET-EXTRA-PRIN SECTION.
      *************************************************************

      * SET THE AMOUNT OF REGULAR PAYMENT TO EXTRA PRINCIPAL

           MOVE REALPYW-POST-REGPY(2) TO REALPYW-POST-EXTRA-PRIN(2)
                                        REALPYW-AP-EXTRA-PRIN(2).
           ADD REALPYW-PDUE-APPRIN(2) TO REALPYW-CURBAL(2).
           ADD REALPYW-CUR-APPRIN(2)  TO REALPYW-CURBAL(2).
           SUBTRACT REALPYW-AP-EXTRA-PRIN(2) FROM REALPYW-CURBAL(2).
           MOVE 0                    TO REALPYW-POST-REGPY(2)
                                        REALPYW-PDUE-APINT(2)
                                        REALPYW-PDUE-APPRIN(2)
                                        REALPYW-CUR-APINT(2)
                                        REALPYW-CUR-APPRIN(2).

           PERFORM REMAIN-INT-PRIN-SINCE-LFP.
           MOVE LFPAP-REMAIN-INT-SINCE-LFP
                                     TO REALPYW-INT-SINCE-LFP(2).
           MOVE LFPAP-REMAIN-PRIN-SINCE-LFP
                                     TO REALPYW-PRIN-SINCE-LFP(2).
           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.

       REALPY-WIN-SET-EXTRA-PRIN-EXIT.
           EXIT.

      *************************************************************
       REALPY-WIN-ORIG-APPLIED SECTION.
      *************************************************************

      * DISPLAY ORIGINAL DISBURSMENT AND SWITCH BACK TO APPLIED

           PERFORM ALARM.
           MOVE 1 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.

           MOVE "S  A070WSEL." TO VDU.
           MOVE " PRESS ANY KEY TO CONTINUE" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010WSELX." TO VDU.
           PERFORM SCREENIO.

           MOVE 2 TO SUBX.
           PERFORM REALPY-WIN-DISPLAY.

       REALPY-WIN-ORIG-APPLIED-EXIT.
           EXIT.

      ******************************************************************
       REALPY-WIN-DISPLAY SECTION.
      ******************************************************************
           MOVE "S  A029TITLE." TO VDU.
           IF SUBX = 1
              MOVE "     DEFAULT POSTING DISTRIBUTION     " TO VDUBUF
           ELSE
              MOVE " REAL ESTATE PAYMENT POSTING RE-APPLY " TO VDUBUF
           END-IF.
           PERFORM SCREENIO.

           MOVE REALPYW-PAYDATE               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D-PAYDATE.
           MOVE REALPYW-TRCD                  TO D-TRCD.
           MOVE REALPYW-REFNO                 TO D-REFNO.
           MOVE REALPYW-TRAMT                 TO D-TRAMT.

           MOVE REALPYW-POST-REGPY(SUBX)      TO D-POST-REGPY.
           MOVE REALPYW-POST-EXTRA-PRIN(SUBX) TO D-POST-EXTRA-PRIN.
           MOVE REALPYW-POST-OTH(SUBX)        TO D-POST-OTH.
           MOVE REALPYW-POST-LCHG(SUBX)       TO D-POST-LCHG.
           ADD REALPYW-POST-REGPY(SUBX) REALPYW-POST-EXTRA-PRIN(SUBX)
               REALPYW-POST-OTH(SUBX)   REALPYW-POST-LCHG(SUBX)
               GIVING D-TOTPMT.


           MOVE REALPYW-PDUE-APPRIN(SUBX)     TO D-PDUE-APPRIN.
           MOVE REALPYW-PDUE-APINT(SUBX)      TO D-PDUE-APINT.
           MOVE REALPYW-CUR-APPRIN(SUBX)      TO D-CUR-APPRIN.
           MOVE REALPYW-CUR-APINT(SUBX)       TO D-CUR-APINT.
           MOVE REALPYW-AP-EXTRA-PRIN(SUBX)   TO D-AP-EXTRA-PRIN.
           MOVE REALPYW-APOTH(SUBX)           TO D-APOTH.
           MOVE REALPYW-APLC(SUBX)            TO D-APLC.
           ADD REALPYW-PDUE-APPRIN(SUBX)   REALPYW-PDUE-APINT(SUBX)
               REALPYW-CUR-APPRIN(SUBX)    REALPYW-CUR-APINT(SUBX)
               REALPYW-AP-EXTRA-PRIN(SUBX) REALPYW-APOTH(SUBX)
               REALPYW-APLC(SUBX)
               GIVING D-TOTAPP.

           MOVE LP-PDTH-DATE                  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D-PDTH.
           MOVE LP-LCPDTH-DATE                TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                   TO D-LCPDTH.
           MOVE REALPYW-OTHBAL(SUBX)          TO D-OTHBAL.
           MOVE REALPYW-LCBAL(SUBX)           TO D-LCBAL.
           MOVE REALPYW-INTBAL(SUBX)          TO D-INTBAL.
           MOVE REALPYW-CURBAL(SUBX)          TO D-CURBAL.
           MOVE REALPYW-INT-SINCE-LFP(SUBX)   TO D-INT-SINCE-LFP.
           MOVE REALPYW-PRIN-SINCE-LFP(SUBX)  TO D-PRIN-SINCE-LFP.

           MOVE REALPY-DISPLAY-LIST TO WR-LIST.
           MOVE REALPY-DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.


      ********************************************************************
       REALPY-GET-TRPROOF SECTION.
      ********************************************************************

      * GET TRANSACTION AMOUNT APPLIED & PROOF

           COMPUTE REALPY-TRAPP = REALPYW-PDUE-APINT(SUBX)
                                + REALPYW-PDUE-APPRIN(SUBX)
                                + REALPYW-CUR-APINT(SUBX)
                                + REALPYW-CUR-APPRIN(SUBX)
                                + REALPYW-AP-EXTRA-PRIN(SUBX)
                                + REALPYW-APOTH(SUBX)
                                + REALPYW-APLC(SUBX).
           COMPUTE REALPY-TRPROOF = REALPY-TRAPP - REALPYW-TRAMT.

      ****************************************************
       REMAIN-INT-PRIN-SINCE-LFP SECTION.
      ****************************************************

      * DETERMINE REMAINING INTEREST SINCE LFP
      * ADJUSTED FOR CHANGE IN APPLIED EXTRA PRINCIPAL

           MOVE REALPYW-CURBAL(2) TO LN-CURBAL.
           PERFORM LFPAP-NEXT-SCHED-PAY.

       REMAIN-INT-PRIN-SINCE-LFP-EXIT.
           EXIT.



       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LFPAP.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/AFPDLLP.CPY".
       COPY "LIBGB/RBACT.CPY".

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           DISPLAY SPACES UPON REALPY-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY REALPY-SCREEN UPON REALPY-WINDOW-HANDLE.
           MOVE REALPY-FORM-FIELDS TO WR-BUF.
           MOVE REALPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE REALPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE REALPY-HELP--DIR TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/REALPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-WSEL.
           MOVE "S  A000WSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-ROUTINE.
      ********************************************
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY REALPY-SCREEN.
           DESTROY REALPY-WINDOW-HANDLE.
           EXIT PROGRAM.
