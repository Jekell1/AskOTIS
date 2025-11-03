       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANH.
      ***********************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER MAINT - PROG 17
      *            -  EARNINGS FOR OTHER
      *            -  ADDON INSURANCE/FEE INFO
      * REVS:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JTG 102496 ---------------ADDED FROM A50---------------------------
      *  JTG 012794 ADDED LOGIC FOR ERNFRMLA = 13, DISCOUNT YIELD - INT
      *                             ERNFRMLA = 57, DISCOUNT YIELD - INT
      *  JTG 062794 ADDED LOGIC FOR ERNFRMLA = 14, DISCOUNT EARNINGS
      *             A % OF PRINCIPAL PAID RE: SP-ERNFRMAL = 6 (FAIRCO)
      *  JTG 072794 SP-ERNFRMLA = 15, REAL INTEREST BEARING METHOD
      *                                               - INSTANT AUTO, FL
      *  JTG 111794 SP-ERNFRMLA = 16, REAL INTEREST BEARING METHOD
      *             DISCOUNT EARNINGS A % OF PRINCIPAL PAID FAIRCO, FL
      *  JTG 031595 ADDED USAGE OF SP-ERNACCRL-DFCD FOR SP-ERNFRMLA(7), CAC
      *            --------------------------------------------------------
      *  JTG 011097 STOPPED SP-ERNFRMLA = 22 DISCOUNT EARN FORMULA,
      *             ATLANTIC FIN BALANCE REMAINING METHOD
      *  JTG 071197 ADDED SP-ERNFRMLA = 64, MERCURY DISCOUNT EARN FORMULA
      *             SAME RATIO AS INTEREST IS EARNED
      *
      *  KEC 090497 ***CONVERSION-A90-TO-A10***
      *                ADDED SP-DUAL-IN-BRANCH, SP-DUAL-IN-STATE.
      *  JTG 010998 ALLOWED ERNFRMAL 62 & 63 AUTOPRIME & MID ATLANTIC #618
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  JTG 990112 ADDED SP-LNCOST-FRMLA AND SP-LNCOST-AMT, REGENCY #006
      *             ALLOWED SETTING UP AN EARNING FORMULA FOR IB ACCOUNTS
      *             NOTE: UPFRONT EARNINGS NOT ALLOWED FOR LOAN COSTS!
      *  JTG 990210 ALLOWED ENTRY OF SP-ERN-DAYS(6)
      *  JTG 990318 ADDED SP-LNCOST-FRMLA 'D',  TFC #008
      *  JTG 000406 ADDED NEW SP-ERNACCRL-FG() = D, NEW SOUTH #1129
      *             EARN AFTER SAME AS CASH PERIOD
      *  JTG 000919 ADDED SP-ADDON-EXCEPT-FG REGACC #71225 PR#1297
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  JTG 010809 ADDED SP-LNCOST-FRMLA-VALID 'P' AMORTIZED
      *             PARTICIPATION EXPENSE                         REGACC PR#1559
      *  JTG 020419 PUT A48 LOGIC PACK AS AN OPTION FOR COMPUTING THE INTEREST
      *             REFUNDS FOR ALL RE: SP-CANCEL-ADDON-FG        COSMO #355
      *  JTG 030620 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      *             ADDED SP-LNCOST-FRMLA-VALID = '3' AMORTIZATED PART. PAID EXP
      *  JTG 040501 ADDED LOGIC TO CAUSE THE UNEARNED LOAN COST TO ROLL TO
      *             THE NEW LOAN AFTER 'RENEWAL'. NOTE: THIS COST IS REFLECTIVE
      *             OF THE DISCOUNT WHEN PURCHASING LOAN PAPER NOT CARRIED AS
      *             SALES PAPER WITH DEALER DISCOUNT.
      *             ADDED SP-LNCOST-FRMLA = 'R'                    REGENCY #2287
      *  JTG 040510 REMOVED LOCK ON DIRECT ONLY            LORI    REGENCY #2287
      *  JTG 040528 CHANGED TO ALLOW DUAL LOANS IN BRANCH  B U T  NOT
      *             DUAL LOANS IN STATE                          LENDMARK #2259
      *  JTG 041015 ADDED HELP MESSAGE FOR ACCRUAL START EARN CODE
      *             WHEN EARNINGS FORMULA IS NOT 51,53,54,OR 56   LENDMARK
      *   CS 041020 ADD CL/SPMEMO WHICH WRITES SPMEMO(SMFILE) WHEN FIELD IS
      *             CHANGED [WORLD PR#362]
      *   CS 070330 ADDED SP-LNCOST-FRMLA [MANUAL INPUT AT LOAN TIME] PR# 2942
      *                                                             COLONIAL
      *   CS 070920 ALLOW SP-LNCOST-FRMLA WHEN LC-SMALL LOAN, SERVICE,SALES =
      *             'N' PER JUDY
      *   CS 110407 ADD SP-LNCOST-FRMLA '4' (SALES ONLY), DOES THE 'F'LAT
      *             AMOUNT IDENTIFIED IN THE LOAN COST AMOUNT, PLUSE TAKES
      *             PARTICIPATION PAID & UPDATES TO LN-ANTICERN(3), FROM A
      *             GL VIEW THIS AMOUNT IS UPDATED TO SERVCHG EARNINGS & THE
      *             ACCRUAL PROVIDES THE AMORTIZATION  REGMGM PR#3765
      *
      * KEC 2013.1107 {PR#00345} REGIONAL ACCEPTANCE <A15> [K.MULLEN]
      *               ADDED SP-LNCOST-FRMLA '5' (SALES ONLY), DOES THE
      *               'F'LAT AMOUNT IDENTIFIED IN THE LOAN COST AMOUNT,
      *               PLUSE TAKES PARTICIPATION PAID & UPDATES TO
      *               LN-ANTICERN(4), FROM A GL VIEW THIS AMOUNT IS
      *               UPDATED TO SERVCHG EARNINGS & THE ACCRUAL
      *               PROVIDES THE AMORTIZATION.
      *
      * MJD 140723 ADDED SP-ADDON-RATE-FG = "A"           MULLEN/KSTREET PR#4556
      * BAH 160422 ADDED SP-DUAL-IN-STATE AND SP-DUAL-IN-BRANCH NEW
      *            VALUE "M" TO NOT CONSIDER AN OPEN REAL ESTATE LOAN AS 
      *            OPEN AND DO NOT CONSIDER A CHARGED OFF LOAN AS OPEN
      *                                              REGENCY PR# 5030
      *  CS 160803 SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20191206 ADDED SP-DUAL-IN-STATE AND SP-DUAL-IN-BRANCH NEW
      *              VALUE "C" TO NOT ALLOW MULTIPLE LOANS WITHIN SAME CLASS
      *              FOR TAX ADVANCE LOANS, WORLD #1407
      **************************************************************************
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
           "@(#)MN/SPMANH.CBL S35 04/11/22-12:06:58 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ELE-OVER-20       PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".

       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.


       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER1
      *    **********************************
       01  DISPLAY-BUF      VALUE SPACES.
           03  A01           PIC Z9.
           03  A02           PIC Z(4).99  BLANK ZERO.
           03  A03           PIC X.
           03  A031          PIC Z(3).99  BLANK ZERO.
           03  A04           PIC X.
           03  A05           PIC X.
           03  A06           PIC X.
           03  A07           PIC Z9.
           03  A08           PIC ZZ9.
           03  A11           PIC X.
           03  A12           PIC Z(4)9.99 BLANK ZERO.
           03  A14           PIC X.
           03  A15           PIC X.
           03  A16           PIC Z9.
           03  A17           PIC 999.
           03  A18           PIC X.
           03  A181          PIC X(11).
           03  A19           PIC ZZ9.999 BLANK ZERO.
           03  A20           PIC X.
           03  A201          PIC X(11).
           03  A21           PIC X.
           03  A211          PIC X(11).
           03  A22           PIC X.
           03  A23           PIC X.
           03  A231          PIC X(11).
           03  A24           PIC X.
           03  A241          PIC X(11).
           03  A25           PIC X.
           03  A251          PIC X(11).
           03  A29           PIC X.
           03  A30           PIC X.
       01  DISPLAY-LIST.
           03  FILLER        PIC X(48)  VALUE
           "010 020 030 031 040 050 060 070 080 110 120 140 ".
           03  FILLER        PIC X(12)  VALUE
           "150 160 170 ".
           03  FILLER        PIC X(48)  VALUE
           "180 181 190 200 201 210 211 220 230 231 240 241 ".
           03  FILLER        PIC X(16)  VALUE
           "250 251 290 300.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99 COMP-3 VALUE 30.
       01  SPEC-TABLE.
      * 1. SP-ERNFRMLA(6)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 2. SP-ERNAMT(6)
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 3. SP-ERNPERCD(6)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 4. SP-ERNACCRL-FG(6)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-ERNACCRL-START-CD(6)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 6. SP-ERNACCRL-DFCD(6)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 9. - 10.  NULL
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      *11. SP-LNCOST-FRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *12. SP-LNCOST-AMT
           03  FILLER        PIC X(8)   VALUE "ENNN052 ".
      *13. NULL
           03  FILLER        PIC X(8)   VALUE "E  A010N".

      *14. SP-DUAL-IN-STATE
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *15. SP-DUAL-IN-BRANCH
           03  FILLER        PIC X(8)   VALUE "E  A010 ".

      * ADDON INFORMATION:
      *16. SP-1STPY-ADDON-DAYS
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      *17. SP-1STPY-ADDON-YRTYPE
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      *18. SP-ADDON-RATE-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *19. SP-ADDON-RATE
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".
      *20. SP-CONSOLIDATE-RT-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *21. SP-ADDON-ORG-MATURITY
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *22. SP-ADDON-WHOLE-FG
           03  FILLER        PIC X(8)   VALUE "E  A010".
      *23. SP-ADDON-DISFRMLA
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      *24. SP-ADDON-EXCEPT-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *25. SP-CANCEL-ADDON-FG
           03  FILLER        PIC X(8)   VALUE "E  A010 ".

           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      * 29 SP-INS-O1-EDIT
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 30 SP-INS-O2-EDIT
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 30 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANH_DEF.CPY".
       COPY "LIBMN/SPMANH_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANH_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANH" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.
       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM REC-SP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

       ENTRY-MODULE-REPEAT.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
             IF SP-LNCOST-FRMLA = "M"
               IF NOT(SP-LNCOST-AMT = ZEROS)
                 MOVE
                "LOAN COST AMOUNT MUST BE ZERO WITH FORMULA 'M'" TO MESS
                                                     PERFORM SEND-MESS
                 GO TO ENTRY-MODULE-REPEAT
               ELSE
                 PERFORM WRITE-MODULE
             ELSE
               PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P2
              MOVE 18  TO FORM-SEL2.

           IF FORM-SEL1 NOT = 0
              IF FORM-P1 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL1 TO FORMSEL
                 GO TO END-ROUTINE.
           IF FORM-SEL2 NOT = 0
              IF FORM-P2 = "P"
                 MOVE SP1-KEY TO SAVEKEY
                 MOVE FORM-SEL2 TO FORMSEL
                 GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *****************************************
       REC-SP-MODULE SECTION.
      *****************************************
       REC-SP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           IF SAVEKEY NOT = " "
              MOVE SAVEKEY TO SP1-KEY
              MOVE " " TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.
           MOVE 0 TO NEW-FLAG.
           MOVE "SPCLASS SUBCLASS LCDESC CLASS STDESC SPNO." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS = "1<"
             PERFORM ENTER-SCANKEY
             GO TO END-REC-SP-MODULE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-MODULE-NO.
           MOVE VDUBUF TO SP-ORGST.
           PERFORM VERIFY-STATE-CODE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
       REC-SP-CLASS.
           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1:"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY TO VDUNUM0.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0        TO SPSCAN-SPRCLASS
              MOVE 0        TO SPSCAN-SUBCLASS
                               SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0
                                            SP-SPRCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-CLASS.
           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.
           IF IO-FG NOT = 0
              GO TO REC-SP-CLASS.
       REC-SP-SUBCLASS.
           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-CLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0           TO SPSCAN-SUBCLASS
                                  SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-SUBCLASS.
           MOVE VDUNUM0 TO SP-SUBCLASS.
       REC-SP-LAWCODE.
           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-SUBCLASS.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-LAWCODE.
           MOVE VDUNUM0 TO SP-LAWCODE.
       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
           MOVE SP-REC TO ORIGINAL-SP-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      *****************************************
       CLASS-SCAN SECTION.
      *****************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *****************************************
       ENTER-SCANKEY SECTION.
      *****************************************
       ENTER-SCAN.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.
           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       ENTER-KEY.
           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE SP-REC TO ORIGINAL-SP-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM VERIFY-STATE-CODE.
           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-LOAN-CLASS.
           PERFORM DISPLAY-SUBCLASS-DESC.
           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           IF LC-LOANTYPE = "I"
              MOVE "N" TO CHFG(21)
              MOVE " " TO SP-ADDON-ORG-MATURITY
           ELSE
              MOVE " " TO CHFG(21).
           IF SP-ADDON-RATE-FG = "F"
              MOVE " " TO CHFG(19)
                          CHFG(21)
                          CHFG(22)
              MOVE "N" TO CHFG(23)
                          CHFG(24)
           ELSE
              MOVE "N" TO CHFG(19)
                          CHFG(21)
                          CHFG(22)
              MOVE " " TO CHFG(23)
                          CHFG(24).

           MOVE " " TO IO-TYPE.
           MOVE 1   TO ONE.
           MOVE 0   TO ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10 THRU 17
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 17
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
           GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

       ENTER-ELE.
           IF ELE = 9 OR 10
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           IF ELE > 20
              SUBTRACT 20 FROM ELE GIVING ELE-OVER-20
              GO TO OVER-20.
           GO TO A-01 A-02 A-03 A-04 A-05
                 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15
                 A-16 A-17 A-18 A-19 A-20
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.
       OVER-20.
           GO TO A-21 A-22 A-23 A-24 A-25
                 A-26 A-27 A-28 A-29 A-30
                 DEPENDING ON ELE-OVER-20.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM0 TO SP-ERNFRMLA(6).
           IF SP-ERN-TERM(6) NOT = 0
                      AND (SP-ERNFRMLA(6) NOT = 53)
              MOVE "FIXED TERM MUST BE 0 UNLESS EARNINGS FORMULA = 53"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ERNDELQFRMLA = "B"
                      AND (SP-ERNFRMLA(6) NOT = 51 AND 0)
              MOVE "MUST BE 51 OR 0 IF DELINQ STOP FRMLA IS B."
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-SERVACCT = "Y" AND SP-ERNFRMLA(6) NOT = 0
              MOVE "NO EARNINGS ON SERVICE ACCOUNTS." TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE.

           IF NOT ( SP-ERNFRMLA-VALID(6) AND
                    ( NOT ( SP-ERNFRMLA(6) =
                                  1 OR 2 OR 3 OR 4 OR 6 OR 55 OR 59
                          )
                    )
                  )
              MOVE "ENTER: 5, 7-10, 51-54, 56-58, 62 OR 64" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

            IF SP-ERNFRMLA(6) = 15 OR 63
               MOVE "THIS FORMULA IS FOR INTEREST ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(6) = 11 OR 13 OR 14 OR 16 OR 22 OR 57
               MOVE "THIS FORMULA IS FOR DEALER DISCOUNT ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      *--------------------------------------------------------------
      *    NOTE: UPFRONT EARNINGS ARE NOT ALLOWED FOR LOAN COSTS
      *          SINCE THE GL DISTRIBUTION WILL WASH OUT EARNINGS.
      *
      *          EX. ASSUME $100.00 LOAN COSTS, SO GL DISTRIBUTION
      *              WILL BE:
      *
      *                ANTICIPATED            INCOME
      *               ---------------     --------------
      *                      | 100.00     100.00 |
      *                      |                   |
      *
      *              IF $20.00 WAS EARNED UPFRONT, THE DISTIBUTION
      *              WOULD BE:
      *
      *                ANTICIPATED            INCOME
      *               ---------------     --------------
      *                      | 80.00       80.00 |
      *                      |                   |
      *              NOTE: NO WAY TO REALIZE $20.00 INCOME (EXPENSE)
      *--------------------------------------------------------------
       A-02.
           MOVE VDUNUM2 TO SP-ERNAMT(6).
           IF ( NEW-FLAG NOT = 1          ) AND
              ( SP-LNCOST-FRMLA NOT = "D" ) AND
              ( SP-ERNAMT(6) NOT = 0      )
                MOVE
                "UPFRONT EARNINGS NOT ALLOWED WITH LOAN COST FORMULA!"
                                                        TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO SP-ERNPERCD(6).
           IF ( NEW-FLAG NOT = 1          ) AND
              ( SP-LNCOST-FRMLA NOT = "D" ) AND
              ( SP-ERNPERCD(6) NOT = " "  )
                MOVE
                "UPFRONT EARNINGS NOT ALLOWED WITH LOAN COST FORMULA!"
                                                        TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           IF NOT SP-ERNPERCD-VALID(6)
              MOVE "ENTER: SPACE, A, OR B" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       A-031.
           MOVE "ENNN032031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-031.
           MOVE VDUNUM2 TO SP-ERNPERCT(6).
           IF ( NEW-FLAG NOT = 1          ) AND
              ( SP-LNCOST-FRMLA NOT = "D" ) AND
              ( SP-ERNPERCT(6) NOT = 0    )
                MOVE
                "UPFRONT EARNINGS NOT ALLOWED WITH LOAN COST FORMULA!"
                                                        TO MESS
                PERFORM SEND-MESS
                GO TO A-031.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-ERNACCRL-FG(6).
           IF NOT SP-ERNACCRL-FG-VALID(6)
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO SP-ERNACCRL-START-CD(6).
           IF NOT (SP-ERNFRMLA(6) = 51 OR 53 OR 54 OR 56)
              IF SP-ERNACCRL-START-CD(6) NOT = " "
                 MOVE
                 "EARNINGS FORMULA REQUIRES NO START CODE, ENTER SPACE"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ERNACCRL-START-CD-VALID(6)
              MOVE "ENTER: SPACE OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-ERNACCRL-DFCD(6).
           IF SP-ERNFRMLA(6) NOT = 7
              IF NOT (SP-ERNACCRL-DFCD(6) = " " OR "A")
                 MOVE "ENTER: SPACE OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (SP-ERNACCRL-DFCD(6) = "0" OR "1" OR "2")
                 MOVE "ENTER: '0' OR '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO SP-ERN-DAYS(6).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO SP-ERN-TERM(6).
           IF SP-ERNFRMLA(6) NOT = 53
                   AND (SP-ERN-TERM(6) NOT = 0)
              MOVE "ONLY ALLOWED WITH FORMULA 53" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           GO TO CREATE-ROUTINE.
       A-10.
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO SP-LNCOST-FRMLA.
           IF NOT SP-LNCOST-FRMLA-VALID
              MOVE " " TO MESS
              STRING "ENTER: ' ', 'F' FIXED, 'A' % LOAN AMT, 'M' MANUAL"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE " " TO MESS
              STRING
                     " 'D' DLCHK, 'P' PARTIC HELD, '3' PARTIC PAID"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE " " TO MESS
              STRING
                     " 'R' ROLL UNERN LNCOST ON RENEWAL"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF LC-SALEFIN NOT = "Y"
              IF SP-LNCOST-FRMLA = "D" OR "P" OR "3" OR "4" OR "5"
                 MOVE
              "CLASS MUST BE SALES  FOR 'D' OR 'P' OR '3' OR '4' OR '5'"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.


           IF ( SP-LNCOST-FRMLA NOT = "D" )
              IF ( SP-ERNAMT(6)   NOT = 0   ) OR
                 ( SP-ERNPERCD(6) NOT = " " ) OR
                 ( SP-ERNPERCT(6) NOT = 0   )
                 MOVE "UPFRONT EARNINGS NOT ALLOWED WITH THIS FORMULA!"
                                                        TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.

      *PER JUDY NEEDS TO WORK WHEN SALES,SERVICE,SMALL LOAN FLAG = 'N'
      *    IF LC-SMALLOAN NOT = "Y"

           IF LC-SALEFIN = "Y" 
              IF SP-LNCOST-FRMLA = "M"
                 MOVE "NOT VALID WITH SALES FINANCE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF SP-LNCOST-FRMLA = "M"
              MOVE "SN N052120." TO VDU
              MOVE ZEROS TO VDUNUM2 SP-LNCOST-AMT
              PERFORM SCREENIO
              MOVE "N" TO CHFG(12).


           GO TO CREATE-ROUTINE.

       A-12.
           MOVE VDUNUM2 TO SP-LNCOST-AMT.
           GO TO CREATE-ROUTINE.
       A-13.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-14.
           MOVE VDUBUF TO SP-DUAL-IN-STATE.

           IF ( SP-DUAL-IN-STATE NOT = "Y" AND NOT = "N"  
                              AND NOT = "M" AND NOT = "C")
              MOVE "ENTER: 'Y', 'N', 'M' OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-15.
           MOVE VDUBUF TO SP-DUAL-IN-BRANCH.

           IF ( SP-DUAL-IN-BRANCH NOT = "Y" AND NOT = "N" 
                              AND NOT = "M" AND NOT = "C")
              MOVE "ENTER: 'Y', 'N', 'M' OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.


           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       A-16.
           MOVE VDUNUM0 TO SP-1STPY-ADDON-DAYS.
           GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM0 TO SP-1STPY-ADDON-YRTYPE.
           IF NOT SP-1STPY-ADDON-YRTYPE-VALID
              MOVE "ENTER: 358, 360, 365, 367, OR 999" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUNUM0 TO SP-ADDON-RATE-FG.
           IF LC-LOANTYPE = "I"
              IF SP-ADDON-RATE-FG NOT = "E"
                 MOVE "IB CLASSES MUST BE AN 'E'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ADDON-RATE-FG-VALID
              MOVE "'F'-FIXED MONTHLY, 'E'-EFFECTIVE RATE, 'A'-APR"
                                                  TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ADDON-RATE-FG = "E" OR "A"
              MOVE "S  A070190." TO VDU
              MOVE 0 TO SP-ADDON-RATE
              PERFORM SCREENIO

              MOVE "S  A010210." TO VDU
              MOVE "U" TO  VDUBUF SP-ADDON-ORG-MATURITY
              PERFORM SCREENIO

              MOVE "S  A010220." TO VDU
              MOVE " " TO VDUBUF SP-ADDON-WHOLE-FG
              PERFORM SCREENIO
              MOVE "S  A010230." TO VDU
              MOVE "C" TO VDUBUF SP-ADDON-DISFRMLA
              PERFORM SCREENIO
              MOVE "N" TO CHFG(19)
                          CHFG(21)
                          CHFG(22)
              MOVE " " TO CHFG(23)
                          CHFG(24)
           ELSE
              MOVE " " TO CHFG(19)
                          CHFG(21)
                          CHFG(22)
              MOVE "N" TO CHFG(23)
                          CHFG(24)
              MOVE "S  A010230." TO VDU
              MOVE " " TO VDUBUF SP-ADDON-DISFRMLA
              PERFORM SCREENIO
              MOVE "S  A010240." TO VDU
              MOVE " " TO VDUBUF SP-ADDON-EXCEPT-FG
              PERFORM SCREENIO
              MOVE "S  A010241." TO VDU
              MOVE " " TO VDUBUF
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUNUM3 TO SP-ADDON-RATE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO SP-CONSOLIDATE-RT-FG.
           IF NOT SP-CONSOLIDATE-RT-FG-VALID
              MOVE "ENTER: SPACE, 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CONSOLIDATE-RT-FG NOT = " "
              MOVE "ENTER: SPACE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO SP-ADDON-ORG-MATURITY.
           IF NOT SP-ADDON-ORG-MATURITY-VALID
              MOVE "ENTER: 'O' OR 'U'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUBUF TO SP-ADDON-WHOLE-FG.
           IF NOT SP-ADDON-WHOLE-FG-VALID
              MOVE "ENTER: ' ' OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUBUF TO SP-ADDON-DISFRMLA.
           IF NOT SP-ADDON-DISFRMLA-VALID
              MOVE "ENTER: 'A'- ACTUARIAL OR 'C' - US RULE"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUBUF TO SP-ADDON-EXCEPT-FG.
           IF NOT (SP-ADDON-EXCEPT-FG = " " OR "T")
              MOVE "ENTER: ' ' OR 'T' - USE TRANSACTION AS START"
                                                      TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE "S  A110241." TO VDU
              MOVE "TRANS START" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A110241." TO VDU
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO SP-CANCEL-ADDON-FG.
           IF NOT (SP-CANCEL-ADDON-FG = " " OR "F")
              MOVE
              "ENTER: ' ' - DEFAULT OR 'F' - FINANCE CANCEL AMT"
                                                      TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-CANCEL-ADDON-FG = "F"
              MOVE "S  A110251." TO VDU
              MOVE "FINANCE AMT" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A110251." TO VDU
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       A-26.
           GO TO CREATE-ROUTINE.
       A-27.
           GO TO CREATE-ROUTINE.
       A-28.
           GO TO CREATE-ROUTINE.
       A-29.
           MOVE VDUBUF TO SP-INS-O1-EDIT.
           IF NOT SP-INS-O1-EDIT-VALID
              MOVE "' '-NONE OR 'A'-1/3.... EDITS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-30.
           MOVE VDUBUF TO SP-INS-O2-EDIT.
           IF NOT SP-INS-O2-EDIT-VALID
              MOVE "' '-NONE OR 'A'-1/3.... EDITS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       WRITE-MODULE SECTION.
      ************************************************
           PERFORM OPEN-SP1-FILE.
           MOVE SP-REC TO CHANGED-SP-REC.
           IF ORIGINAL-SP-REC = CHANGED-SP-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF SP-REC NOT = ORIGINAL-SP-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-SP-REC TO SP-REC.
           IF LC-LOANTYPE = "I"
              MOVE " " TO SP-ADDON-ORG-MATURITY.
           PERFORM REWRITE-SP1-FILE.
      *WRITES A SPR MEMO WHEN CHANGES OCCUR
           IF IO-GOOD
              MOVE FORM-PATHNAME TO FORM-PATH
              MOVE "CL/SPMEMO"   TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   ORIGINAL-SP-REC
                                   SP-REC
              CANCEL FORM-PROGX.

      * CODE TO WRITE CHANGE FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE SP1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      ************************************************
       SPSCAN-SCAN SECTION.
      ************************************************
           MOVE SPACES TO SPSCAN-SP1-KEY.
           MOVE SPACES TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SPSCAN-SCAN
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SPSCAN-SCAN.
           EXIT.

      ************************************************
       DISPLAY-FIELDS SECTION.
      ************************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-ERNFRMLA(6) TO A01.
           MOVE SP-ERNAMT(6) TO A02.
           MOVE SP-ERNPERCD(6) TO A03.
           MOVE SP-ERNPERCT(6) TO A031.
           MOVE SP-ERNACCRL-FG(6) TO A04.
           MOVE SP-ERNACCRL-START-CD(6) TO A05.
           MOVE SP-ERNACCRL-DFCD(6) TO A06.
           MOVE SP-ERN-DAYS(6) TO A07.
           MOVE SP-ERN-TERM(6) TO A08.

           MOVE SP-LNCOST-FRMLA TO A11.
           MOVE SP-LNCOST-AMT   TO A12.

           MOVE SP-DUAL-IN-STATE  TO A14.
           MOVE SP-DUAL-IN-BRANCH TO A15.

           MOVE SP-1STPY-ADDON-DAYS   TO A16.
           MOVE SP-1STPY-ADDON-YRTYPE TO A17.
           MOVE SP-ADDON-RATE-FG      TO A18.
           MOVE SP-ADDON-RATE         TO A19.
           MOVE SP-CONSOLIDATE-RT-FG  TO A20.
           MOVE SP-ADDON-ORG-MATURITY TO A21.
           MOVE SP-ADDON-WHOLE-FG     TO A22.
           MOVE SP-ADDON-DISFRMLA     TO A23.
           MOVE SP-ADDON-EXCEPT-FG    TO A24.
           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE "TRANS START" TO A241
           ELSE
              MOVE " "           TO A241.
           MOVE SP-CANCEL-ADDON-FG    TO A25.
           IF SP-CANCEL-ADDON-FG = "F"
              MOVE "FINANCE AMT" TO A251
           ELSE
              MOVE " "           TO A251.
           MOVE SP-INS-O1-EDIT        TO A29.
           MOVE SP-INS-O2-EDIT        TO A30.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANH-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANH-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANH-FORM-FIELDS TO WR-BUF.
           MOVE SPMANH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANH-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANH-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO ST-DESC.
           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       IO-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************************
       END-ROUTINE SECTION.
      *************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANH-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
