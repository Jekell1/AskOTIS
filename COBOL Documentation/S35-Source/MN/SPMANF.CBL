       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANF.
      ******************************************************************
      *   DIR :  MN
      *   DESC:  STATE PARAMETER MAINT - PROG 15
      *
      *          EARNINGS FOR ADDON INS. CHARGES AND SERVICE
      * REVS:
      *  SLC 110289 ADDED LOGIC FOR TFC EARNINGS REVERTING TO CASH FROM
      *                   ACCRUAL.
      *  SLC 110689 ADDED EARNINGS FORMULA 10!!
      *  JTG 121389 LOCKED OUT 1 - 15 FOR FUTURE FASB EXPENSES
      *  SLC 032290 MODIFIED FOR ADDONS
      *  JTG 050490 VERIFIED NO BUNDLE OF EARNINGS
      *  JTG 081090 OPENED UP INPUT OF EARNING FRMLA FOR ADDON INS F/C
      *  SLC 091990 ADDED FIXED TERM EARNINGS
      *  JTG 021891 ADDED SP-ERNACCRL-START-CD(X) = 'B', MULLEN.
      *  JTG 123091 CHANGED FOR SP-ERNFRMLA = 11, INVESTORS FINANCIAL(VA)
      *  BAH 022892 REMOVED ADDON JUNK
      *  JTG 091892 ADDED LOGIC FOR SP-ERNFRMLA = 56
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  JTG 081395 ADDED SP-ERNFRMLA = 60, MERCURY ACTUARIAL (ELAPSED DAYS)
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  ----102496-------------- TAKEN FROM A50 ---------------------------
      *  JTG 012794 ADDED LOGIC FOR ERNFRMLA = 13, DISCOUNT YIELD - INT
      *                             ERNFRMLA = 57, DISCOUNT YIELD - INT
      *  JTG 062794 ADDED LOGIC FOR ERNFRMLA = 14, DISCOUNT EARNINGS
      *             A % OF PRINCIPAL PAID RE: SP-ERNFRMAL = 6 (FAIRCO)
      *  JTG 072794 SP-ERNFRMLA = 15, REAL INTEREST BEARING METHOD
      *                                              - INSTANT AUTO, FL
      *  JTG 020795 ADDED ERNFRMLA 17 AUTOPRIME FL
      *  JTG 031595 ADDED USAGE OF SP-ERNACCRL-DFCD FOR SP-ERNFRMLA(7), CAC
      *  -------------------------------------------------------------------
      *  JTG 011097 STOPPED SP-ERNFRMLA = 22 DISCOUNT EARN FORMULA,
      *             ATLANTIC FIN BALANCE REMAINING METHOD
      *  JTG 071197 ADDED SP-ERNFRMLA = 64, MERCURY DISCOUNT EARN FORMULA
      *             SAME RATIO AS INTEREST IS EARNED
      *  JTG 010998 ALLOWED ERNFRMAL 62 & 63 AUTOPRIME & MID ATLANTIC #618
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  JTG 000406 ADDED NEW SP-ERNACCRL-FG() = D, NEW SOUTH #1129
      *             EARN AFTER SAME AS CASH PERIOD
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  JTG 010521 NEW CODE 'T' FOR SP-ERNPERCD-VALID-SC          PAACO PR#1469
      *  JTG 010522 NEW CODE '23' FOR SP-ERNFRMLA                  PAACO PR#1469
      *  JTG 041015 ADDED HELP MESSAGE FOR ACCRUAL START EARN CODE
      *             WHEN EARNINGS FORMULA IS NOT 51,53,54,OR 56   LENDMARK
      *   CS 041020 ADD CL/SPMEMO WHICH WRITES SPR MEMO(SMFILE) WHEN FIELD IS
      *             CHANGED [WORLD PR#362]
      *   CS 121121 ADD SP-ERNDELQFRMLA "R", RECALCULATES EARNINGS ON CASH
      *             PAYS WHEN ACCOUNT CONTRACTUAL REACHES SP-ERNDELQ-STOP. 'R'
      *             CAN ONLY BE USED WHEN EARNINGS COMPONENT FRMLA IS 52
      *             ATTEMPTED TO STOP WITH A VERIFY MODULE THAT WHEN YOU ENTER
      *             'R', ALL EARNINGS FRMLA'S MUST BE 52. WORLD PR#828
      *   CS 121214 ONLY SC/MF/INT MUST BE EARNINGS 52  WORLD PR#838
      *   CS 121217 ONLY SC/MF/INT MUST BE 52 IF LC-LOANTYPE IS PRECOMP, IF IB
      *             ONLY SC MUST BE 52 WHEN USING SP-ERNDELQFRMLA 'R' PR#838
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
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      * BLM 210802 FIX BLOWING UP, WAS MOVING USER-ID TO BR-NO INSTEAD OF
      *            EXT-CONAME-USER-LOC
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
           "@(#)MN/SPMANF.CBL S35 04/14/22-14:31:30 >".
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
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       01  DISPLAY-LISTS.
           03  FILLER        PIC X(44)  VALUE
           "010 020 030 031 040 050 060 070 080 160 170 ".
           03  FILLER        PIC X(28)  VALUE
           "180 181 190 200 210 220 230.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                             PIC X(68).

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 99 COMP-3 VALUE 30.
       01  SPEC-TABLE.

      * INS. ADDON FC INCOME:
      * 1. SP-ERNFRMLA(2)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 2. SP-ERNAMT(2)
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 3. SP-ERNPERCD(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 4. SP-ERNACCRL-FG(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 5. SP-ERNACCRL-START-CD(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 6. SP-ERNACCRL-DFCD(2)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
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

      * SERVICE CHARGE:
      * 16. SP-ERNFRMLA(3)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 17. SP-ERNAMT(3)
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
      * 18. SP-ERNPERCD(3)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 19. SP-ERNACCRL-FG(3)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 20. SP-ERNACCRL-START-CD(3)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 21. SP-ERNACCRL-DFCD(3)
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 24. - 30.  NULL
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
           03  FILLER        PIC X(8)   VALUE "E  A010N".
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
       COPY "LIBMN/SPMANF_DEF.CPY".
       COPY "LIBMN/SPMANF_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANF_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ***************************************
       MAIN-PROGRAM SECTION.
      ***************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "N" TO BR-INS-ADDON
           ELSE
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 MOVE "N" TO BR-INS-ADDON.
           IF BR-INS-ADDON = "N"
              MOVE "N" TO CHFG(1)  CHFG(2)  CHFG(3)  CHFG(4)
                          CHFG(5)  CHFG(6)  CHFG(7)  CHFG(8)
                          CHFG(9)  CHFG(10) CHFG(12) CHFG(13)
                          CHFG(14) CHFG(15).
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANF" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.


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
              MOVE 16 TO FORM-SEL2.
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

      ***************************************
       REC-SP-MODULE SECTION.
      ***************************************
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
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0           TO SPSCAN-LAWCODE
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
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.
       END-REC-SP-MODULE.
           EXIT.

      ***************************************
       CLASS-SCAN SECTION.
      ***************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

       EXIT-CLASS-SCAN.
           EXIT.

      ***************************************
       ENTER-SCANKEY SECTION.
      ***************************************
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
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 15
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
           IF SP-ERNFRMLA(2) = 56
              MOVE "N" TO CHFG(4)
           ELSE
              MOVE " " TO CHFG(4).
           IF SP-ERNFRMLA(3) = 56
              MOVE "N" TO CHFG(19)
           ELSE
              MOVE " " TO CHFG(19).

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
           MOVE VDUNUM0 TO SP-ERNFRMLA(2).
           IF SP-ERN-TERM(2) NOT = 0
                       AND (SP-ERNFRMLA(2) NOT = 53)
              MOVE "FIXED TERM MUST BE 0 UNLESS EARNINGS FORMULA = 53"
                 TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NOT (SP-ERNFRMLA(2) = 56 OR 51 OR 0)
              MOVE "ENTER 0 OR 51 OR 56" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-SERVACCT = "Y" AND SP-ERNFRMLA(2) NOT = 0
              MOVE "NO EARNINGS ON SERVICE ACCOUNTS." TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

            IF SP-ERNFRMLA(2) = 15
               MOVE "THIS FORMULA IS FOR INTEREST ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(2) = 17 OR 64
               MOVE "THIS IS NOT FOR ADDON INTEREST!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(2) = 11 OR 13 OR 14 OR 16 OR 22 OR 57
               MOVE "THIS FORMULA IS FOR DEALER DISCOUNT ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(2) = 56
               MOVE "S  A010040." TO VDU
               PERFORM SCREENIO
               MOVE " " TO SP-ERNACCRL-FG(2).

            GO TO CREATE-ROUTINE.

       A-02.
           MOVE VDUNUM2 TO SP-ERNAMT(2).
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUBUF TO SP-ERNPERCD(2).
           IF NOT SP-ERNPERCD-VALID(2)
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
           MOVE VDUNUM2 TO SP-ERNPERCT(2).
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUBUF TO SP-ERNACCRL-FG(2).
           IF NOT SP-ERNACCRL-FG-VALID(2)
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUBUF TO SP-ERNACCRL-START-CD(2).
           IF NOT (SP-ERNFRMLA(2) = 51 OR 53 OR 54 OR 56)
              IF SP-ERNACCRL-START-CD(2) NOT = " "
                 MOVE
                 "EARNINGS FORMULA REQUIRES NO START CODE, ENTER SPACE"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ERNACCRL-START-CD-VALID(2)
              MOVE "ENTER: SPACE OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUBUF TO SP-ERNACCRL-DFCD(2).
           IF SP-ERNFRMLA(2) NOT = 7
              IF NOT (SP-ERNACCRL-DFCD(2) = " " OR "A")
                 MOVE "ENTER: SPACE OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (SP-ERNACCRL-DFCD(2) = "0" OR "1" OR "2")
                 MOVE "ENTER: '0' OR '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUNUM0 TO SP-ERN-DAYS(2).
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO SP-ERN-TERM(2).
           IF SP-ERNFRMLA(2) NOT = 53
                     AND (SP-ERN-TERM(2) NOT = 0)
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
           MOVE VDUNUM0 TO SP-ERNFRMLA(3).
           IF SP-ERNDELQFRMLA = "R"
               IF NOT (SP-ERNFRMLA(3) = 52)
                 MOVE "EARNINGS MUST BE 52 WITH DELINQ STOP FRMLA 'R'"
                                       TO MESS
                  PERFORM SEND-MESS
                  GO TO ENTER-ELE.

           IF SP-ERNFRMLA(3) = 65
              IF SP-ERNDELQFRMLA NOT = " "
                MOVE "WARNING: DELINQ STOP FORMULA MUST BE SPACES"
                                                       TO MESS
                PERFORM SEND-MESS 
                GO TO ENTER-ELE 
              ELSE
                MOVE 65 TO SP-ERNFRMLA(1)
                MOVE 65 TO SP-ERNFRMLA(4)
                MOVE "INTEREST AND M/F EARNINGSMUST BE 65" TO MESS
                PERFORM SEND-MESS
                MOVE "SETTING INTEREST, M/F EARNINGS TO 65" TO MESS
                PERFORM SEND-MESS.

           IF SP-ERN-TERM(3) NOT = 0
                      AND (SP-ERNFRMLA(3) NOT = 53)
              MOVE "FIXED TERM MUST BE 0 UNLESS EARNINGS FORMULA = 53"
                                                           TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ERNDELQFRMLA = "B"
                       AND (SP-ERNFRMLA(3) NOT = 51 AND 0)
              MOVE "MUST BE 51 OR 0 IF DELINQ STOP FRMLA IS B."
                                                          TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF LC-SERVACCT = "Y" AND SP-ERNFRMLA(3) NOT = 0
              MOVE "NO EARNINGS ON SERVICE ACCOUNTS." TO MESS
              PERFORM SEND-MESS
              GO TO CREATE-ROUTINE.

           IF NOT SP-ERNFRMLA-VALID(3)
              MOVE "ENTER: 0-10, 23, 51-56, 58-60, 62, 64, 65" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF SP-RBFRMLA(7) = "D" OR "F"
              IF SP-ERNFRMLA(1) = 1 OR 2 OR 3 OR 4 OR 59
                 IF (SP-ERNFRMLA(1) NOT = SP-ERNFRMLA(3)
                               AND SP-ERNFRMLA(3) NOT = 0)
                    MOVE
                 "INT AND SCHG EARN FORMULAS MUST BE THE SAME OR BLANK."
                                                             TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                 IF NEW-FLAG NOT = 1
                    IF SP-ERNFRMLA(1) NOT = SP-ERNFRMLA(2)
                                AND (SP-ERNFRMLA(2) NOT = 0)
                       MOVE
              "INT AND ADDON EARN FORMULAS MUST BE THE SAME OR BLANK."
                                                             TO MESS
                       PERFORM SEND-MESS
                       MOVE 1 TO ELE
                       GO TO ENTER-ELE.

            IF SP-ERNFRMLA(3) = 11 OR 13 OR 14 OR 16 OR 22 OR 57
               MOVE "THIS FORMULA IS FOR DEALER DISCOUNT ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(3) = 15 OR 63
               MOVE "THIS FORMULA IS FOR INTEREST ONLY!" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.

            IF SP-ERNFRMLA(3) = 56
               MOVE "S  A010190." TO VDU
               PERFORM SCREENIO
               MOVE " " TO SP-ERNACCRL-FG(3).

            GO TO CREATE-ROUTINE.
       A-17.
           MOVE VDUNUM2 TO SP-ERNAMT(3).
           GO TO CREATE-ROUTINE.
       A-18.
           MOVE VDUBUF TO SP-ERNPERCD(3).
           IF NOT SP-ERNPERCD-VALID-SC(3)
              MOVE "ENTER: SPACE, A, B OR T" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       A-181.
           MOVE "ENNN032181." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO A-181.
           MOVE VDUNUM2 TO SP-ERNPERCT(3).
           GO TO CREATE-ROUTINE.
       A-19.
           MOVE VDUBUF TO SP-ERNACCRL-FG(3).
           IF NOT SP-ERNACCRL-FG-VALID(3)
              MOVE "ENTER: SPACE, 'A', 'B', 'C' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-20.
           MOVE VDUBUF TO SP-ERNACCRL-START-CD(3).
           IF NOT (SP-ERNFRMLA(3) = 51 OR 53 OR 54 OR 56)
              IF SP-ERNACCRL-START-CD(3) NOT = " "
                 MOVE
                 "EARNINGS FORMULA REQUIRES NO START CODE, ENTER SPACE"
                 TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF NOT SP-ERNACCRL-START-CD-VALID(3)
              MOVE "ENTER: SPACE OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-21.
           MOVE VDUBUF TO SP-ERNACCRL-DFCD(3).
           IF SP-ERNFRMLA(3) NOT = 7
              IF NOT (SP-ERNACCRL-DFCD(3) = " " OR "A")
                 MOVE "ENTER: SPACE OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF NOT (SP-ERNACCRL-DFCD(3) = "0" OR "1" OR "2")
                 MOVE "ENTER: '0' OR '1' OR '2'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-22.
           MOVE VDUNUM0 TO SP-ERN-DAYS(3).
           GO TO CREATE-ROUTINE.
       A-23.
           MOVE VDUNUM0 TO SP-ERN-TERM(3).
           IF SP-ERNFRMLA(3) NOT = 53
                   AND (SP-ERN-TERM(3) NOT = 0)
              MOVE "ONLY ALLOWED WITH FORMULA 53" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-24.
           GO TO CREATE-ROUTINE.
       A-25.
           GO TO CREATE-ROUTINE.
       A-26.
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

           MOVE SP-ERNFRMLA(2) TO A01.
           MOVE SP-ERNAMT(2) TO A02.
           MOVE SP-ERNPERCD(2) TO A03.
           MOVE SP-ERNPERCT(2) TO A031.
           MOVE SP-ERNACCRL-FG(2) TO A04.
           MOVE SP-ERNACCRL-START-CD(2) TO A05.
           MOVE SP-ERNACCRL-DFCD(2) TO A06.
           MOVE SP-ERN-DAYS(2) TO A07.
           MOVE SP-ERN-TERM(2) TO A08.

           MOVE SP-ERNFRMLA(3) TO A16.
           MOVE SP-ERNAMT(3) TO A17.
           MOVE SP-ERNPERCD(3) TO A18.
           MOVE SP-ERNPERCT(3) TO A181.
           MOVE SP-ERNACCRL-FG(3) TO A19.
           MOVE SP-ERNACCRL-START-CD(3) TO A20.
           MOVE SP-ERNACCRL-DFCD(3) TO A21.
           MOVE SP-ERN-DAYS(3) TO A22.
           MOVE SP-ERN-TERM(3) TO A23.
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
           IF SP-ERNFRMLA(3) = 0
              IF SP-SCFRMLA NOT = 0
                 MOVE "EARNINGS FORMULA FOR SERVCHG REQUIRED!"
                                                     TO MESS
                 PERFORM SEND-MESS
                 MOVE "CLR" TO IO-TYPE.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMANF-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANF-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANF-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANF-FORM-FIELDS TO WR-BUF.
           MOVE SPMANF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMANF-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANF-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANF_EVA.CPY".
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

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANF-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
