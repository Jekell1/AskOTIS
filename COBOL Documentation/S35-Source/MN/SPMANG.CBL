       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANG.
      ******************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER MAINT - PROG 16
      *            MAINTENANCE FEES AND
      *            EARNINGS FOR DEALER DISCOUNT
      * REVS:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  JTG 081395 ADDED SP-ERNFRMLA = 60, MERCURY ACTUARIAL (ELAPSED DAYS)
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JTG 102496 ------------------ADDED FROM A50--------------------------
      *   JTG 012794 ADDED LOGIC FOR ERNFRMLA = 13, DISCOUNT YIELD - INT
      *                              ERNFRMLA = 57, DISCOUNT YIELD - INT
      *   JTG 062794 ADDED LOGIC FOR ERNFRMLA = 14, DISCOUNT EARNINGS
      *              A % OF PRINCIPAL PAID RE: SP-ERNFRMAL = 6 (FAIRCO)
      *   JTG 072794 SP-ERNFRMLA 15, REAL INTEREST BEARING METHOD
      *                                               - INSTANT AUTO, FL
      *   JTG 111794 SP-ERNFRMLA = 16, REAL INTEREST BEARING METHOD
      *              DISCOUNT EARNINGS A % OF PRINCIPAL PAID FAIRCO, FL
      *   JTG 020795 ADDED ERNFRMLA 17 AUTOPRIME FL
      *   JTG 031595 ADDED USAGE OF SP-ERNACCRL-DFCD FOR SP-ERNFRMLA(7), CAC
      *             ------------------ADDED FROM A50--------------------------
      *   JTG 011097 ADDED SP-ERNFRMLA = 22 DISCOUNT EARN FORMULA, ATLANTIC FIN
      *                    BALANCE REMAINING METHOD
      *  JTG 071197 ADDED SP-ERNFRMLA = 64, MERCURY DISCOUNT EARN FORMULA
      *             SAME RATIO AS INTEREST IS EARNED
      *  JTG 010998 ALLOWED ERNFRMAL 62 & 63 AUTOPRIME & MID ATLANTIC #618
      *  JTG 050898 STOPPED INPUT OF DISCOUNT ERNFRMLA = 6
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  JTG 990317 ADDED SP-DLDISC-RVPRIN1-PCNT,                       TFC #008
      *                   SP-LNCOST-FRMLA-VALID VALUE "R"
      *  JTG 991214 ADDED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      *  JTG 000331 ALLOWED SP-ERNFRMLA(1) = 63 & SP-ERNFRMLA(5) = 51, ETC
      *                                                      MID ALANTIC #J008
      *  JTG 000406 ADDED NEW SP-ERNACCRL-FG() = D, NEW SOUTH #1129
      *             EARN AFTER SAME AS CASH PERIOD
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  JTG 030711 ALLOWED ENTRY TO FIELD 7 SP-ERN-ADD-DAYS FOR MAINT. FEES
      *             RE: JUDY CONVERSION TO A15                             JTGX
      *  JTG 041015 ADDED HELP MESSAGE FOR ACCRUAL START EARN CODE
      *             WHEN EARNINGS FORMULA IS NOT 51,53,54,OR 56   LENDMARK
      *   CS 041020 ADD CL/SPMEMO WHICH WRITES SPR MEMO(SMFILE) WHEN FIELD IS
      *             CHANGED [WORLD PR#362]
      *   CS 071213 ALLOW MF EARNINGS FRMLA IF BR-USE-PARTIC-FLAT-TABLE = 'Y'
      *             & ITS A SALES CLASS. ITS NOT REALLY A MF, JUST ANOTHER WAY
      *             TO AMORTIZE PARTIC. HELD PR#185 REGACC
      *  MJD 080923 ADDED GPENV LOGIC.  ADDED TEST OF GP-ALLOW-MF-ON-IB
      *             WHEN ENTERING MFEE EARNINGS FORMULA.         WORLD PR#590
      *   CS 090713 IF GP-I-AM-WORLD, ALLOW DISCOUNT EARNINGS FORMULA, WORLD
      *             WILL USE THIS BUCKET FOR A 4% RESERVE ON BULK PURCHASES.
      *             THERE WILL BE NO ACCESS TO THE DISCOUNT FIELD, THIS IS
      *             JUST A MECHANISM TO AMORTIZE THE RESERVE PR#643
      *   CS 090729 ADDED SP-DLDISC-CONV-BF-PCNT, WORLD NEEDS TO AMORTIZE A
      *             RESERVE FOR BULK PURCHASES PUT ON THRU CONVERSION. THIS
      *             PERCENT WILL BE MULTIPLIED BY THE PURCHASE BALANCE AND
      *             STORED IN LN-ANTICERN(5)  WORLD PR# 648
      *   CS 121120 ADD SP-ERNDELQFRMLA  'R', RECALCULATES EARNINGS ON CASH
      *             PAYS WHEN ACCOUNT CONTRACUTAL REACHES SP-ERNDELQ-STOP. IF
      *             'R', ALL EARNINGS FRMLA'S MUST BE 52 WORLD PR#828
      *   CS 121214 ONLY SC/MF/INT MUST BE EARNINGS 52 TO USE SP-ERNDELQFRMLA R
      *                                                 WORLD PR#838
      *   CS 121217 ONLY SC/MF/INT MUEST BE 52 IF LC-LOANTYPE IS PRECOMP, IF
      *             IB ONLY SC MUST BE 52 WHEN USING SP-ERNDELQFRMLA 'R'
      *                                                     WRLD PR#838
      *   CS 130919 ADD SP-ERNFRMLA 65 (WORLD ONLY). THIS WILL BE USED IN
      *             A BEOM ENVIRONMENT TO EVALUATE THE DIFFERENCES BETWEEN 52
      *             ACCRUAL AND A 52 ACCRUAL THAT USES CASH PAYMENTS TO SET
      *             THE ACCRUAL DATE.  PR#930
      *   CS 141203 FIXED BUG WITH ALWAYS WRITING THE RECORD WHEN CHANGES WERE
      *             MADE, NO TEST ON IO-TYPE = "RWR" SO EVEN AN F1 WROTE THE
      *             RECORD. THIS WAS FOUND WHEN SAFECO WAS TRYING TO DETERMINE
      *             WHY SERVCHG WAS NOT TAKEN INTO EARNINGS ON A PAYOFF, DUE TO
      *             OTHER AND SC HAVING SAME LN-EARN-ORDER. I TRIED TO MAKE
      *             DUPLICATE EARN ORDER IN SPR & IT GAVE ME A MESSAGE THAT
      *             IT WASNT ALLOWED BUT IT WROTE THE RECORD! THE CHANGE THAT
      *             CREATED THIS WAS FROM 11/19/12. THE STOP FORMULA OF 'R'  
      *             REQUIRES INT/SC/MF EARNINGS TO BE 52. WAS TRYING TO 
      *             VERIFY THAT IN A VERIFY MODULE FOR EACH COMPONENT & MISSED
      *             TEST ON IO-TYPE. CHANGED TO EDIT ON INPUT & ALSO IF STOP
      *             FORMULA IS "R", THE EARNINGS COMPONENTS ARE SET TO 52
      *             AND TESTED AGAIN ON EACH INDIVIDUAL PAGE IF YOU ATTEMPT TO
      *             CHANGE THEM.
      *  CS 160803 SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 160808 CHANGED SPSCAN-SP1-KEY TO SPSCAN-SP1-KEY
      *  CS 170505 ACTIVATE 8 DIGIT DATES
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
           "@(#)MN/SPMANG.CBL S34 06/08/21-10:54:54 >".
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
           03  A16           PIC Z9.
           03  A17           PIC Z(4).99   BLANK ZERO.
           03  A18           PIC X.
           03  A181          PIC Z(3).99   BLANK ZERO.
           03  A19           PIC X.
           03  A20           PIC X.
           03  A21           PIC X.
           03  A22           PIC Z9.
           03  A23           PIC ZZ9.
           03  A24           PIC Z(3).99  BLANK ZERO.
           03  A25           PIC Z(3).99  BLANK ZERO.
           03  A26           PIC Z(3).999  BLANK ZERO.
       01  DISPLAY-LIST.
           03  FILLER        PIC X(44)  VALUE
           "010 020 030 031 040 050 060 070 080 160 170 ".
           03  FILLER        PIC X(40)  VALUE
           "180 181 190 200 210 220 230 240 250 260.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99 COMP-3 VALUE 30.
       01  SPEC-TABLE.
      * 1. SP-ERNFRMLA(4)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 2. SP-ERNAMT(4)
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 3. SP-ERNPERCD(4)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 4. SP-ERNACCRL-FG(4)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-ERNACCRL-START-CD(4)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 6. SP-ERNACCRL-DFCD(4)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      *    03  FILLER        PIC X(8)   VALUE "ENNN020N". 
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 9. - 15.  NULL
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
      * 16. SP-ERNFRMLA(5)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 17. SP-ERNAMT(5)
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 18. SP-ERNPERCD(5)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 19. SP-ERNACCRL-FG(5)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 20. SP-ERNACCRL-START-CD(5)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 21. SP-ERNACCRL-DFCD(5)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 24. SP-DLDISC-RVPRIN1-PCNT
           03  FILLER        PIC X(8)   VALUE "ENNN032 ".
      * 25. SP-DLDISC-RVPRIN1-AF-PCNT
           03  FILLER        PIC X(8)   VALUE "ENNN032 ".
      * 26. SP-DLDISC-CONV-BF-PERCENT
           03  FILLER        PIC X(8)   VALUE "ENNN033 ".

      * 27. - 30.  NULL
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".


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
       COPY "LIBMN/SPMANG_DEF.CPY".
       COPY "LIBMN/SPMANG_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANG_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANG" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

       MAIN-MODULE.
           PERFORM FORM-RESET.
           PERFORM REC-SP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P2
              MOVE 17 TO FORM-SEL2.
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


      **********************************************
       REC-SP-MODULE SECTION.
      **********************************************
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
              MOVE 0 TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
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
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
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
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0 TO SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.

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
           PERFORM GET-LC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      **********************************************
       CLASS-SCAN SECTION.
      **********************************************
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

      **********************************************
       ENTER-SCANKEY SECTION.
      **********************************************
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
           PERFORM GET-LC.
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

      ************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
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
              MOVE SPACES TO IO-TYPE
              PERFORM VERIFY-EARN-FIELDS
              IF IO-TYPE = "CLR"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10 THRU 16
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 16
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHECK-CHNO.
       CHECK-PAGE.
           MOVE SPACES TO IO-TYPE.
           PERFORM VERIFY-EARN-FIELDS.
           IF IO-TYPE = "CLR"
              GO TO MODIFY-ROUTINE
           ELSE
              MOVE "RWR" TO IO-TYPE
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
           IF SP-ERNFRMLA(4) = 56
              MOVE "N" TO CHFG(4)
           ELSE
              MOVE " " TO CHFG(4).
           IF SP-ERNFRMLA(5) = 56
              MOVE "N" TO CHFG(19)
           ELSE
              MOVE " " TO CHFG(19).

           IF ELE = 9
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
           MOVE VDUNUM0 TO SP-ERNFRMLA(4).
           IF SP-ERNDELQFRMLA = "R"
               IF NOT (SP-ERNFRMLA(4) = 52)
                  MOVE "EARNINGS MUST BE 52 WITH DELINQ STOP FRMLA 'R'"
                                       TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-ELE.

           IF SP-ERNFRMLA(4) = 65
             IF SP-ERNDELQFRMLA NOT = " "
                MOVE
                "WARNING: DELINQ STOP FORMULA MUST BE SPACES"
                                              TO MESS
                PERFORM SEND-MESS 
                GO TO ENTER-ELE 
              ELSE
                MOVE 65 TO SP-ERNFRMLA(1)
                MOVE 65 TO SP-ERNFRMLA(3)
                MOVE "INTEREST AND M/F MUST BE 65" TO MESS
                PERFORM SEND-MESS
                MOVE "SETTING INTEREST AND M/F TO 65" TO MESS
                PERFORM SEND-MESS.

           IF SP-ERN-TERM(4) NOT = 0
                      AND (SP-ERNFRMLA(4) NOT = 53)
              MOVE "FIXED TERM MUST BE 0 UNLESS EARNINGS FORMULA = 53"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ERNDELQFRMLA = "B" AND
              SP-ERNFRMLA(4) NOT = 51 AND 0
              MOVE "MUST BE 51 OR 0 IF DELINQ STOP FRMLA IS B."
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-SERVACCT = "Y" AND SP-ERNFRMLA(4) NOT = 0
              MOVE "NO EARNINGS ON SERVICE ACCOUNTS." TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE.


           IF NOT SP-ERNFRMLA-VALID(4)
              MOVE "ENTER: 0-10,12,15,19,21,51-56,58-60,62-63,65"
                                                         TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-LOANTYPE = "I"
              IF (LC-SALEFIN NOT = "Y")
                IF SP-ERNFRMLA(4) NOT = 0
                   IF NOT (GP-ALLOW-MF-ON-IB = "Y")
                 MOVE "ENTER: '0' FOR IB" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.


      *    IF LC-LOANTYPE = "I"
      *       IF SP-ERNFRMLA(4) NOT = 0
      *          MOVE "ENTER: '0' FOR IB" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-ELE.

           IF SP-ERNFRMLA(4) = 15 OR 63
              MOVE "THIS FORMULA IS FOR INTEREST ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF SP-ERNFRMLA(4) = 11 OR 13 OR 14 OR 16 OR 22 OR 57
              MOVE "THIS FORMULA IS FOR DEALER DISCOUNT ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

            IF SP-ERNFRMLA(4) = 56
               MOVE "S  A010040." TO VDU
               PERFORM SCREENIO
               MOVE " " TO SP-ERNACCRL-FG(4).

            GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM2 TO SP-ERNAMT(4).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO SP-ERNPERCD(4).
           IF NOT SP-ERNPERCD-VALID(4)
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
           MOVE VDUNUM2 TO SP-ERNPERCT(4).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-ERNACCRL-FG(4).
           IF NOT SP-ERNACCRL-FG-VALID(4)
              MOVE "ENTER: SPACE, 'A' OR 'B' OR 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO SP-ERNACCRL-START-CD(4).
           IF NOT (SP-ERNFRMLA(4) = 51 OR 53 OR 54 OR 56)
              IF SP-ERNACCRL-START-CD(4) NOT = " "
                 MOVE
                 "EARNINGS FORMULA REQUIRES NO START CODE, ENTER SPACE"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ERNACCRL-START-CD-VALID(4)
              MOVE "ENTER: SPACE OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-ERNACCRL-DFCD(4).
           IF SP-ERNFRMLA(4) NOT = 7
              IF NOT (SP-ERNACCRL-DFCD(4) = " " OR "A")
                 MOVE "ENTER: SPACE OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (SP-ERNACCRL-DFCD(4) = "0" OR "1" OR "2")
                 MOVE "ENTER: '0' OR '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO SP-ERN-DAYS(4).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO SP-ERN-TERM(4).
           IF SP-ERNFRMLA(4) NOT = 53
                     AND (SP-ERN-TERM(4) NOT = 0)
              MOVE "ONLY ALLOWED WITH FORMULA 53" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-09.
           GO TO CREATE-ROUTINE.
       A-10.
           GO TO CREATE-ROUTINE.
       A-11.
           GO TO CREATE-ROUTINE.
       A-12.
           GO TO CREATE-ROUTINE.
       A-13.
           GO TO CREATE-ROUTINE.
       A-14.
           GO TO CREATE-ROUTINE.
       A-15.
           GO TO CREATE-ROUTINE.
       A-16.
           MOVE VDUNUM0 TO SP-ERNFRMLA(5).
           IF SP-ERNFRMLA(5) = 65
              MOVE "EARNINGS FORMULA NOT AVAILABLE" TO MESS
                                           PERFORM SEND-MESS
              GO TO ENTER-ELE.


           IF SP-ERN-TERM(5) NOT = 0
                      AND (SP-ERNFRMLA(5) NOT = 53)
              MOVE "FIXED TERM MUST BE 0 UNLESS EARNINGS FORMULA = 53"
                                                               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ERNDELQFRMLA = "B"
                     AND (SP-ERNFRMLA(5) NOT = 51 AND 0)
              MOVE "MUST BE 51 OR 0 IF DELINQ STOP FRMLA IS B."
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

            IF SP-ERNFRMLA(1) = 15
               IF SP-ERNFRMLA(5) NOT = 15
                  MOVE "WHEN INTEREST EARN FORMULA IS '15'," TO MESS
                  PERFORM SEND-MESS
                  MOVE "DISCOUNT EARN FORMULA MUST BE '15'." TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-ELE.

            IF SP-ERNFRMLA(4) = 63
               MOVE "THIS FORMULA IS FOR INTEREST ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

           IF LC-SERVACCT = "Y" AND SP-ERNFRMLA(5) NOT = 0
              MOVE "NO EARNINGS ON SERVICE ACCOUNTS." TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE.

           IF NOT (SP-ERNFRMLA-VALID(5))
                       OR (SP-ERNFRMLA(5) =
                           1 OR 2 OR 3 OR 4 OR 6 OR 59)
              MOVE "ENTER: 5, 7-17 OR 22 OR 51-58 OR 60 OR 62 OR 64"
                                                            TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF SP-ERNFRMLA(1) > 50
              IF SP-ERNFRMLA(5) = 11
                 MOVE "NOT ALLOWED, INT ERNFRMLA IS ACCRUAL METHOD"
                 TO MESS PERFORM SEND-MESS
                 GO TO ENTER-ELE.
            IF SP-ERNFRMLA(5) = 56
               MOVE "S  A010190." TO VDU
               PERFORM SCREENIO
               MOVE " " TO SP-ERNACCRL-FG(5).

      * J008 JTG UNNECESSARY LOGIC, RE: MID ALANTIC 000331
      *     IF SP-ERNFRMLA(1) = 63
      *        IF NOT (SP-ERNFRMLA(5) = 0 OR 63)
      *           MOVE
      *        "DISC. FORMULA MUST BE 63, TO MATCH INTEREST FORMULA"
      *                                                   TO MESS
      *        PERFORM SEND-MESS
      *        GO TO ENTER-ELE.

            IF SP-ERNFRMLA(5) = 63
               IF SP-ERNFRMLA(1) NOT = 63
                  MOVE
               "INTEREST FORMULA MUST ALSO BE 63!!!!!!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO SP-ERNAMT(5).
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO SP-ERNPERCD(5).
           IF NOT SP-ERNPERCD-VALID(5)
              MOVE "ENTER: SPACE, A, OR B" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       A-181.
           MOVE "ENNN032181." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-181.
           MOVE VDUNUM2 TO SP-ERNPERCT(5).
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO SP-ERNACCRL-FG(5).
           IF NOT SP-ERNACCRL-FG-VALID(5)
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO SP-ERNACCRL-START-CD(5).
           IF NOT (SP-ERNFRMLA(5) = 51 OR 53 OR 54 OR 56)
              IF SP-ERNACCRL-START-CD(5) NOT = " "
                 MOVE
                 "EARNINGS FORMULA REQUIRES NO START CODE, ENTER SPACE"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ERNACCRL-START-CD-VALID(5)
              MOVE "ENTER: SPACE OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO SP-ERNACCRL-DFCD(5).
           IF SP-ERNFRMLA(5) NOT = 7
              IF NOT (SP-ERNACCRL-DFCD(5) = " " OR "A")
                 MOVE "ENTER: SPACE OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (SP-ERNACCRL-DFCD(5) = "0" OR "1" OR "2")
                 MOVE "ENTER: '0' OR '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO SP-ERN-DAYS(5).
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO SP-ERN-TERM(5).
           IF SP-ERNFRMLA(5) NOT = 53 AND
              SP-ERN-TERM(5) NOT = 0
              MOVE "ONLY ALLOWED WITH FORMULA 53" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
      * ONLY ALLOW ENTRY OF SP-DLDISC-RVPRIN1-PCNT IF SALES FINANCE CLASS:
           IF LC-SALEFIN NOT = "Y"
              IF VDUNUM2 NOT = 0
                 MOVE " ENTER ZERO FOR NON SALES FINANCE CLASSES"
                                                          TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              IF VDUNUM2 NOT = 0
                 IF SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0
                    MOVE " ONLY ONE RVPRIN1% CAN NOT BE ZERO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUNUM2 TO SP-DLDISC-RVPRIN1-PCNT.
           GO TO CREATE-ROUTINE.
       A-25.
      * ONLY ALLOW ENTRY OF SP-DLDISC-RVPRIN1-AF-PCNT IF SALES FINANCE CLASS:
           IF LC-SALEFIN NOT = "Y"
              IF VDUNUM2 NOT = 0
                 MOVE " ENTER ZERO FOR NON SALES FINANCE CLASSES"
                                                          TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
           ELSE
              IF VDUNUM2 NOT = 0
                 IF SP-DLDISC-RVPRIN1-PCNT NOT = 0
                    MOVE " ONLY ONE RVPRIN1% CAN NOT BE ZERO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE VDUNUM2 TO SP-DLDISC-RVPRIN1-AF-PCNT.
           GO TO CREATE-ROUTINE.
       A-26.
      * WORLD NEEDED A METHOD TO AMORTIZE A  RESERVE ON MANUAL BULK PURCHASES
      * PUT ON THE SYSTEM USING CONVERSION & UPDATE LN-ANTICERN(5). USE THIS
      * PERCENT * BALANCE AT CONVERSION FOR BOTH DIRECT AND SALES FINANCE
           IF SP-ERNFRMLA(5) = ZERO
              IF VDUNUM3 NOT = ZERO
                 MOVE "DISCOUNT CONV PERCT NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
      * MOVE THE ZERO
                 MOVE VDUNUM3 TO SP-DLDISC-CONV-BF-PCNT
           ELSE
              MOVE VDUNUM3 TO SP-DLDISC-CONV-BF-PCNT.

           GO TO CREATE-ROUTINE.

       A-27.
           GO TO CREATE-ROUTINE.
       A-28.
           GO TO CREATE-ROUTINE.
       A-29.
           GO TO CREATE-ROUTINE.
       A-30.
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

           MOVE SP-ERNFRMLA(4)            TO A01.
           MOVE SP-ERNAMT(4)              TO A02.
           MOVE SP-ERNPERCD(4)            TO A03.
           MOVE SP-ERNPERCT(4)            TO A031.
           MOVE SP-ERNACCRL-FG(4)         TO A04.
           MOVE SP-ERNACCRL-START-CD(4)   TO A05.
           MOVE SP-ERNACCRL-DFCD(4)       TO A06.
           MOVE SP-ERN-DAYS(4)            TO A07.
           MOVE SP-ERN-TERM(4)            TO A08.

           MOVE SP-ERNFRMLA(5)            TO A16.
           MOVE SP-ERNAMT(5)              TO A17.
           MOVE SP-ERNPERCD(5)            TO A18.
           MOVE SP-ERNPERCT(5)            TO A181.
           MOVE SP-ERNACCRL-FG(5)         TO A19.
           MOVE SP-ERNACCRL-START-CD(5)   TO A20.
           MOVE SP-ERNACCRL-DFCD(5)       TO A21.
           MOVE SP-ERN-DAYS(5)            TO A22.
           MOVE SP-ERN-TERM(5)            TO A23.
           MOVE SP-DLDISC-RVPRIN1-PCNT    TO A24.
           MOVE SP-DLDISC-RVPRIN1-AF-PCNT TO A25.
           MOVE SP-DLDISC-CONV-BF-PCNT    TO A26.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      ************************************************
       VERIFY-EARN-FIELDS SECTION.
      ************************************************
           MOVE SPACE TO IO-TYPE.
           PERFORM VERIFY-EARN-SPLIT.

      ************************************************
       VERIFY-EARN-SPLIT SECTION.
      ************************************************
           IF SP-ERNFRMLA(4) = 0
              IF SP-MFFRMLA NOT = 0
                 MOVE "EARNINGS FORMULA FOR MAINTFEE REQUIRED!"
                                                     TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANG-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANG-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANG-FORM-FIELDS TO WR-BUF.
           MOVE SPMANG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANG-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANG-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GPENV.CPY".

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
           PERFORM GET-LC.
           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       GET-LC.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC
              MOVE "N" TO LC-SALEFIN.
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

      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
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

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANG-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
