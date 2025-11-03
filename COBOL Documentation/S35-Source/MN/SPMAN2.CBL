       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMAN2.
      ******************************************************************
      *   DIR : MN
      *   DESC: STATE PARAMETER MAINT - PROG 2
      *
      * REV:
      *  JTG 072688 ALLOWED SP-DEFFRMLA 6
      *  JTG 121388 ADDED SP-DEFPERCD 6 IOWA
      *  JTG 090589 ALLOWED SP-DEFBASE 6 (GEORGIA)
      *  SLC 112289 ALLOWED DEF FORMULAS 7 AND 99 (SPRING)
      *  SLC 032290 MODIFIED FOR ADDONS
      *  JTG 052590 ALLOWED SP-DEFFRMLA 8 (IN) PACESETTER
      *  BAH 022892 REMOVED ADDON JUNK
      *  JTG 063092 ADDED SP-DEFPERCD 7 COLONIAL TN
      *                   SP-DEFBASE 7
      *  MJD 050895 ADDED CODE TO MAKE CHANGE FILE AFTER REWRITE
      *  MJD 051895 ADDED F6 - FORM SCAN OPTION
      *  SLC 062495 ADDED SP-DEFBASE 8 (WHOLE REMAINING PAYMENTS)
      *  JFF 120195 ADDED CLASS SCAN & STATE PARAMETER SCAN
      *  MJD 011096 ADDED SP-MAX-ALLOW-DELINQ.
      *  JFF 070896 ADDED SP-DEFUNITPER-CD (PAYFREQ CD).
      *  BAH 071796 CHANGED SP-DEFPERCD 1 TO BE APR DIVIDED BY
      *             LN-UNITPER-PER-YEAR INSTEAD OF FORCING 12
      *  BAH 072496 ADDED SP-DEFPERCD = 8 (INSTANT AUTO)
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  BAH 081098 ADDED SP-YTD-MAX-ALLOW-DELINQ AND SP-ALLOW-DELINQ-POLCD
      *             FOR MERCURY'S NEW EDIT ON ALLOWABLE DELINQUENCY, #193
      *  MJD 991220 ADDED SP-DEFPOLCD = "B" (WORLD PR#216)
      *  JTG 000320 NEW DEFERMENT FORMULA SP-DEFFRMLA (> OF INTDUE OR 1/3 OF
      *             REGPAY) MERCURY #224
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  MG  010326 ADDED F5-KEY OPTION TO TOGGLE SP-PRFORM
      *             1~10 AND 11~20.
      *  MG  010406 TOOK OUT {(A) & (B)} IF THE FORMS WERE 11~20.
      *  CS  041001 ADD CALL TO CL/SPMEMO WHICH WILL WRITE A SPR CHANGE MEMO
      *                                                          TYPE 'A'
      *  BAH 060214 ADDED 25. SP-INTERIM-COUPON FOR SALES FINANCE ACC PR# 2704
      *   CS 070709 ADDED NEW IB DEFFRMLA 96 2% OF BALANCE MULLEN PR#2477
      *  BAH 081118 ADDED NEW IB DEFFRMLA 95 HAMPTON ROADS PR# 3280
      *  MJD 131113 A30 CONVERSION COMPLETED.
      *  BAH 131017 ADDED SP-DEFPOLCD 'C' FOR HONOR PR# 4382
      *  MJD 150512 ADDED SP-DEFBASE = 11                          REGENCY #4753
      *
      * KEC 2016.0211 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *  MJD 161213 ADDED DEFERMENT FORMULA 10 (71225 PR#384)
      *   CS 170505 ACTIVATE 8 DIGIT DATES
      *  MJD 170331 ADDED SP-ALT-DEFFRMLA, SP-ALT-DEFMIN AND SP-ALT-DEFMAX.
      *             ONLY DISPLAY AND ALLOW CHANGES IF GP-I-AM-REGACC.
      *             USE FX/FIXSP8.C TO SET THEM.  THESE WILL BE USED FOR
      *             ALTERNATIVE DEFERMENT CALCULATION IN WI/COLAS2     71225#386
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
           "@(#)MN/SPMAN2.CBL S34 06/06/21-16:54:16 >".
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
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

       01  SEL-TYPE           VALUE SPACES.
           03  SEL-FMNO       PIC 9(4).
           03  SEL-FMNOX  REDEFINES SEL-FMNO
                              PIC X(4).

       01  ONE               PIC S9.
       01  S                 PIC 99     COMP-3.
       01  S1                PIC 99     COMP-3.
       01  S2                PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
       COPY "LIBLP/SPMANSEL.CPY".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  FIRST-SCAN        PIC 9      VALUE 0.

       01  HOLD-PRFORM-10    PIC X(15).
       01  HOLD-PRFORM-20    PIC X(15).

       01  HOLD-A-FIELD      PIC X(03).
       01  HOLD-B-FIELD      PIC X(03).

       01  ORIGINAL-SP-REC      PIC X(SP-SIZE) VALUE SPACES.
       01  CHANGED-SP-REC       PIC X(SP-SIZE) VALUE SPACES.

       01  CURRENT-PRFORM    PIC 9(02)   VALUE 10.

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01           PIC Z9.
           03  F011          PIC X.
           03  F0111         PIC Z9.
           03  F012          PIC X.
           03  F02           PIC Z9.
           03  F03           PIC Z9.
           03  F04           PIC Z9.9999  BLANK ZERO.
           03  F05           PIC Z9.99    BLANK ZERO.
           03  F051          PIC Z9.99    BLANK ZERO.
           03  F06           PIC ZZZ9.99  BLANK ZERO.
           03  F061          PIC ZZZ9.99  BLANK ZERO.
           03  F07           PIC X.
           03  F08           PIC ZZ.
           03  F09           PIC ZZ.
           03  F10           PIC ZZ.
           03  F11           PIC X.
           03  F12           PIC ZZ.
           03  F13           PIC ZZ.
           03  DISPLAY-PRFORM-BUF.
               05  F-PRFORM-10   PIC X(15).
               05  F-PRFORM-20   PIC X(15).
               05  F-A           PIC X(03).
               05  F-B           PIC X(03).
               05  F-FORMS       OCCURS 10.
                   07  F-FORM    PIC ZZZ.
                   07  F-DESC    PIC X(21).
                   07  F-FG      PIC X.
           03  F24           PIC X.
           03  F25           PIC X.

       01  DISPLAY-LIST.
           03  FILLER        PIC X(33)  VALUE
           "010 011 0111 012 020 030 040 050 ".
           03  FILLER        PIC X(24)  VALUE
           "051 060 061 070 080 090 ".
           03  FILLER        PIC X(16)  VALUE
           "100 110 120 130 ".

           03  DISPLAY-PRFORM-LIST.
               05  PRFORM-TOGGLE-ATTR.
                   07  FILLER          PIC X(09)  VALUE "PRFORM-10".
                   07  PRFORM-10-ATTR  PIC X(04)  VALUE ":RF ".
                   07  FILLER          PIC X(09)  VALUE "PRFORM-20".
                   07  PRFORM-20-ATTR  PIC X(04)  VALUE "    ".
                   07  FILLER          PIC X(10)  VALUE "A-FIELD:S ".
                   07  FILLER          PIC X(10)  VALUE "B-FIELD:S ".

           03  FILLER        PIC X(44)  VALUE
               "140 141 142 150 151 152 160 161 162 170 171 ".
           03  FILLER        PIC X(32)  VALUE
               "172 180 181 182 190 191 192 200 ".
           03  FILLER        PIC X(44)  VALUE
               "201 202 210 211 212 220 221 222 230 231 232 ".
           03  FILLER        PIC X(8)  VALUE
               "240 250.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX          PIC 99 COMP-3 VALUE 25.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN024 ".
      * 5. SP-DEFMIN
           03  FILLER        PIC X(8)   VALUE "ENNN022 ".
           03  FILLER        PIC X(8)   VALUE "ENNN042 ".
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 10. SP-DEFPOLMX(3)
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 11. SP-ALLOW-DELINQ-POLCD
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 12. SP-YTD-MAX-ALLOW-DELINQ
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 13. SP-MAX-ALLOW-DELINQ
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
      * 14. SP-PRFORM
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
           03  FILLER        PIC X(8)   VALUE "ENNN030 ".
      * 24. SP-IMED-REC
           03  FILLER        PIC X(8)   VALUE "E  A010 ".
      * 25. SP-INTERIM-COUPON
           03  FILLER        PIC X(8)   VALUE "E  A010 ".

       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 25 TIMES.
               05  SPEC     PIC X(7).
               05  CHFG     PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBWI/FDSCANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMAN2_DEF.CPY".
       COPY "LIBMN/SPMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  FORM-LINK-AREA PIC X(2000).
       SCREEN SECTION.
       COPY "LIBMN/SPMAN2_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *************************************************
      *      MAIN PROGRAM MODULE
      *************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMAN2" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

           MOVE FORM-LINK-AREA TO SPMAIN-LINK.
           MOVE "FORMS 01 ~ 10" TO HOLD-PRFORM-10.
           MOVE "FORMS 11 ~ 20" TO HOLD-PRFORM-20.
           MOVE "(A)" TO HOLD-A-FIELD.
           MOVE "(B)" TO HOLD-B-FIELD.
           CALL "C$TOLOWER" USING HOLD-A-FIELD, VALUE 03.
           CALL "C$TOLOWER" USING HOLD-B-FIELD, VALUE 03.
           MOVE 10 TO CURRENT-PRFORM.
           
           MOVE "S  A0090111P." TO VDU.
           MOVE "         " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A0020111." TO VDU.
           MOVE "  " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A005051." TO VDU.
           MOVE "     " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A007061." TO VDU.
           MOVE "       " TO VDUBUF.
           PERFORM SCREENIO.

       MAIN-MODULE.
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

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
              MOVE "P" TO FORM-P1
              MOVE 3 TO FORM-SEL1.
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

      *******************************************
       REC-SP-MODULE SECTION.
      *******************************************
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
           PERFORM CALL-WRFORM..
      *    CALL FRESET USING STAT MESS.
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

      *******************************************
       CLASS-SCAN SECTION.
      *******************************************
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

      *********************************************
       ENTER-SCANKEY SECTION.
      *********************************************
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

      ***********************************************
      *   ENTRY-MODULE
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
           IF ( F5-KEY ) PERFORM TOGGLE-PRFORM.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF FORM-SEL1 < 0 OR FORM-SEL1 = 2
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 14
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.
       CHK-CHNO.
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
       TEST-F6.
           IF VDUSTATUS = "1<"
              IF ELE > 13
                 IF ELE < 24
                    GO TO GOTO-FIELD.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUNUM0 TO SP-DEFFRMLA.
           IF NOT SP-DEFFRMLA-VALID
              MOVE "ENTER: 0 THRU 10 OR 95 THRU 99" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-DEFFRMLA = 99 OR 98 OR 97 OR 96 OR 95
              IF LC-LOANTYPE NOT = "I"
                 MOVE "INTEREST BEARING CLASSES ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-DEFFRMLA = 0 OR 1
              IF LC-LOANTYPE = "I"
                 MOVE "PRE COMPUTE ACCOUNTS ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF SP-DEFFRMLA NOT = 0
              MOVE "SNNN020020." TO VDU
              MOVE 0 TO SP-DEFBASE VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A200021." TO VDU
              PERFORM SCREENIO
              MOVE "SNNN020030." TO VDU
              MOVE 0 TO SP-DEFPERCD VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A200031." TO VDU
              PERFORM SCREENIO
              MOVE "SNNN024040." TO VDU
              MOVE 0 TO SP-DEFWORKER VDUNUM0
              PERFORM SCREENIO.

           IF NOT (SP-DEFFRMLA = 2 OR 4 OR 5 OR 6)
              MOVE SPACES TO SP-DEFEXCEPT
              MOVE "S  A010011." TO VDU
              PERFORM SCREENIO
              GO TO A-012.
       A-011.
           MOVE "ENTER EXCEPTION FLAG" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A010011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           MOVE VDUBUF TO SP-DEFEXCEPT.
           IF NOT SP-DEFEXCEPT-VALID
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO A-011.
           IF SP-DEFFRMLA NOT = 0
              MOVE 4 TO ELE.
           GO TO CREATE-ROUTINE.

       A-012.
           MOVE "ENTER DEFERMENT PAY FREQUENCY CODE:" TO MESS.
           PERFORM SEND-MESS.
           MOVE "E  A010012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              IF NOT (SP-DEFFRMLA = 2 OR 4 OR 5 OR 6 )
                 GO TO ENTER-ELE
              ELSE
                 GO TO A-011.
           MOVE VDUBUF TO SP-DEFUNITPER-CD.
           IF NOT (SP-DEFUNITPER-CD = " " OR "A")
              MOVE "ENTER: SPACE OR 'A'" TO MESS
              PERFORM SEND-MESS
              GO TO A-012.
           IF SP-DEFFRMLA NOT = 0
              MOVE 4 TO ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO SP-DEFBASE.
           IF SP-DEFFRMLA NOT = 0 AND SP-DEFBASE NOT = 0
              MOVE "NO ENTRY UNLESS FIELD 1 IS ZERO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NOT SP-DEFBASE-VALID
              MOVE "ENTER: 0 THRU 11" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SEND-DEFBASE-DESC.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO SP-DEFPERCD.
           IF SP-DEFFRMLA NOT = 0 AND SP-DEFPERCD NOT = 0
              MOVE "NO ENTRY UNLESS FIELD 1 IS ZERO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NOT SP-DEFPERCD-VALID
              MOVE "ENTER: 0 THRU 8" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SEND-DEFPERCD-DESC.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM4 TO SP-DEFWORKER.
           IF SP-DEFFRMLA NOT = 0 AND SP-DEFWORKER NOT = 0
              MOVE "NO ENTRY UNLESS FIELD 1 IS ZERO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDUNUM2 TO SP-DEFMIN.
           GO TO CREATE-ROUTINE.
       A-051.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE VDUNUM2 TO SP-DEFMAX.
           GO TO CREATE-ROUTINE.
       A-061.
           GO TO CREATE-ROUTINE.
       A-07.
           MOVE VDUBUF TO SP-DEFPOLCD.
           IF NOT SP-DEFPOLCD-VALID
              MOVE "ENTER: SPACE, 'A', 'B', OR 'C'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           PERFORM SEND-MAXIMUM-DESC.
           GO TO CREATE-ROUTINE.
       A-08.
           MOVE VDUNUM0 TO SP-DEFPOLMX(1).
           GO TO CREATE-ROUTINE.
       A-09.
           MOVE VDUNUM0 TO SP-DEFPOLMX(2).
           GO TO CREATE-ROUTINE.
       A-10.
           MOVE VDUNUM0 TO SP-DEFPOLMX(3).
           GO TO CREATE-ROUTINE.
       A-11.
           MOVE VDUBUF TO SP-ALLOW-DELINQ-POLCD.
           IF NOT SP-ALLOW-DELINQ-POLCD-VALID
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-12.
           MOVE VDUNUM0 TO SP-YTD-MAX-ALLOW-DELINQ.
           GO TO CREATE-ROUTINE.
       A-13.
           MOVE VDUNUM0 TO SP-MAX-ALLOW-DELINQ.
           GO TO CREATE-ROUTINE.
       A-14.
           PERFORM ENTER-FORMNO
           GO TO CREATE-ROUTINE.
       A-15.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-16.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-17.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-18.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-19.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-20.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-21.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-22.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-23.
           PERFORM ENTER-FORMNO.
           GO TO CREATE-ROUTINE.
       A-24.
           MOVE VDUBUF TO SP-IMED-REC.
           IF NOT SP-IMED-REC-VALID
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-25.
           MOVE VDUBUF TO SP-INTERIM-COUPON.
           IF SP-INTERIM-COUPON NOT = "Y" AND NOT =  "N" AND
                                 NOT = " "
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      **********************************************
       ENTER-FORMNO SECTION.
      **********************************************
           IF VDUSTATUS = "1<"
               PERFORM FORM-SCAN
               IF IO-FG NOT = 0
                  GO TO ENTER-FMNO
               ELSE
                  MOVE SEL-FMNO TO VDUNUM0
               END-IF
           END-IF.

      * ADJUST ELE TO SUBSCRIPT TABLE
           IF ( CURRENT-PRFORM = 10 )
              SUBTRACT 13 FROM ELE GIVING S
              GO TO ENTER-FMNO-SET-PRFORM
           ELSE
           IF ( CURRENT-PRFORM = 20 )
              SUBTRACT 03 FROM ELE GIVING S
              GO TO ENTER-FMNO-SET-PRFORM.

       ENTER-FMNO.
           MOVE "ENNN030  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" AND NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO ENTER-FORMNO-EXIT.
           IF VDUSTATUS = "1D"
              MOVE SP-PRFORM(S) TO VDUNUM0
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1<"
              GO TO ENTER-FORMNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FMNO.
       ENTER-FMNO-SET-PRFORM.
           MOVE VDUNUM0 TO SP-PRFORM(S).
           IF VDUNUM0 = 0
              PERFORM CLEAR-FORMS
              GO TO ENTER-FORMNO-EXIT.

           MOVE VDUNUM0 TO FD-FMNO.
           MOVE "S  N030  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE FD-FMNO TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VALIDATE-FORMNO.
           IF IO-FG = 9
              GO TO ENTER-FMNO.

           MOVE "S  A021  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE FD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       ENTER-PRFLAG.
           MOVE "E  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FMNO.
           IF VDUSTATUS = "1D"
              MOVE SP-PRFLAG(S) TO VDUBUF
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PRFLAG.
           MOVE VDUBUF TO SP-PRFLAG(S).
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              MOVE "ENTER: 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRFLAG.
       ENTER-FORMNO-EXIT.
           EXIT.

      ****************************************
       CLEAR-FORMS SECTION.
      ****************************************
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           MOVE 1 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 2 TO VDUCOL.
           PERFORM SCREENIO.
           MOVE 0 TO SP-PRFORM(S).
           MOVE " " TO SP-PRFLAG(S).

      * ONLY CLEAR CURRENT LEVEL OF TABLE

       CLEAR-FORMS-EXIT.
           EXIT.

      *******************************************
       WRITE-MODULE SECTION.
      *******************************************
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

      *************************************************
       SPSCAN-SCAN SECTION.
      *************************************************
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

      *************************************************
       DISPLAY-FIELDS SECTION.
      *************************************************
           MOVE "S  A0090111P." TO VDU.
           MOVE "         " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE SP-DEFFRMLA TO F01.
           MOVE SP-DEFEXCEPT TO F011.
           MOVE SP-DEFUNITPER-CD TO F012.
           MOVE SP-DEFBASE TO F02.
           MOVE SP-DEFPERCD TO F03.
           MOVE SP-DEFWORKER TO F04.
           MOVE SP-DEFMIN TO F05.
           MOVE SP-DEFMAX TO F06.
           MOVE SP-DEFPOLCD TO F07.
           MOVE SP-DEFPOLMX(1) TO F08.
           MOVE SP-DEFPOLMX(2) TO F09.
           MOVE SP-DEFPOLMX(3) TO F10.
           MOVE SP-ALLOW-DELINQ-POLCD TO F11.
           MOVE SP-YTD-MAX-ALLOW-DELINQ TO F12.
           MOVE SP-MAX-ALLOW-DELINQ TO F13.
           
           MOVE 0 TO F0111 F051 F061.

      *- - - - - - - - - - - - - - - - - - - - - - - - -{ SETUP PRFORM }
           MOVE HOLD-PRFORM-10 TO F-PRFORM-10.
           MOVE HOLD-PRFORM-20 TO F-PRFORM-20.
           IF ( CURRENT-PRFORM = 10 )
              MOVE HOLD-A-FIELD   TO F-A
              MOVE HOLD-B-FIELD   TO F-B
           ELSE
              MOVE SPACES         TO F-A
              MOVE SPACES         TO F-B.

           IF ( CURRENT-PRFORM = 10 )
              MOVE ZEROES TO S1
           ELSE
              MOVE 10     TO S1.

           PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 10
                   ADD 1 TO S1
                   MOVE SP-PRFLAG(S1) TO F-FG  (S2)
                   MOVE SP-PRFORM(S1) TO F-FORM(S2) FD-FMNO
                   IF ( SP-PRFORM(S1) NOT = ZEROES )
                      PERFORM VALIDATE-FORMNO
                      IF ( IO-GOOD )
                         MOVE FD-DESC   TO F-DESC(S2)
                      ELSE
                         MOVE "INVALID" TO F-DESC(S2)
                      END-IF
                   END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       DISPLAY-FIELDS-CONTINUE.

           MOVE SP-IMED-REC TO F24.
           MOVE SP-INTERIM-COUPON TO F25.
           PERFORM SEND-DEFPERCD-DESC.
           PERFORM SEND-DEFBASE-DESC.
           PERFORM SEND-MAXIMUM-DESC.

      *- - - - - - - - - - - - - - - - - - -{ SET ATTRIBUTE FOR PRFORM }
           IF ( CURRENT-PRFORM = 10 )
              MOVE ":RF " TO PRFORM-10-ATTR
              MOVE SPACES TO PRFORM-20-ATTR
           ELSE
              MOVE SPACES TO PRFORM-10-ATTR
              MOVE ":RF " TO PRFORM-20-ATTR.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *************************************************
       FORM-SCAN SECTION.
      *************************************************
           MOVE 0   TO FDSCAN-KEY.
           MOVE "C" TO FDSCAN-FMTYPE.
           MOVE "S" TO FDSCAN-ALLOW-CHG.
           MOVE "WI/FDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH FDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF FDSCAN-STATUS NOT = "00" OR FDSCAN-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-FORM-SCAN.
           MOVE "00" TO VDUSTATUS.
           MOVE FDSCAN-KEY-ALP TO SEL-FMNOX.

       EXIT-FORM-SCAN.
           EXIT.

       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ***************************************************************
       FORM-RESET SECTION.
      ***************************************************************
           MOVE EXT-CONAME-CONAME TO SPMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMAN2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMAN2-SCREEN.
           DISPLAY SPMAN2-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMAN2-FORM-FIELDS TO WR-BUF.
           MOVE SPMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           MOVE SPMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMAN2-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       MISC-PARAGRAPHS SECTION.
      *************************************************
       COPY "LIBGB/ACCESS.CPY".
       TOGGLE-PRFORM.
           IF ( CURRENT-PRFORM = 10 )
              MOVE 20 TO CURRENT-PRFORM
              PERFORM DISPLAY-FIELDS
           ELSE
              MOVE 10 TO CURRENT-PRFORM
              PERFORM DISPLAY-FIELDS.
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
       SEND-MAXIMUM-DESC.
           MOVE "S  A120081." TO VDU.
           IF SP-DEFPOLCD = "A"
              MOVE "PER CONTRACT" TO VDUBUF
           ELSE
              MOVE "PER YEAR " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A120091." TO VDU.
           MOVE "PER CONTRACT" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A120101." TO VDU.
           IF SP-DEFPOLCD = "A"
              MOVE "MONTH BREAK" TO VDUBUF
           ELSE
              MOVE "NOT USED" TO VDUBUF.
           PERFORM SCREENIO.
       SEND-DEFBASE-DESC.
           MOVE "S  A120021." TO VDU.
           IF SP-DEFFRMLA NOT = 0
              MOVE SPACE TO VDUBUF
           ELSE
           IF SP-DEFBASE = 0
              MOVE "REG PAY AMT" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 1
              MOVE "WRP * REG PY" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 2
              MOVE "PRIN BALANCE" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 3
              MOVE "WRP*PY - FEE" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 4
              MOVE "INT+SERV+EXT+MF" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 5
              MOVE "INT + EXT CHG" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 6
              MOVE "REGPAY - MF" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 7
              MOVE "SCHDPAY*RMTERM" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 8
              MOVE "WHOLE REM INST" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 9
              MOVE "NET BALANCE   " TO VDUBUF
           ELSE
           IF SP-DEFBASE = 10
              MOVE "NET BAL + 5.00" TO VDUBUF
           ELSE
           IF SP-DEFBASE = 11
              MOVE "PMT-MF * TRM" TO VDUBUF
           ELSE
              MOVE "INVALID" TO VDUBUF.
           PERFORM SCREENIO.
       SEND-DEFPERCD-DESC.
           MOVE "S  A120031." TO VDU.
           IF SP-DEFFRMLA NOT = 0
              MOVE SPACE TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 0
              MOVE "% IN WORKER " TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 1
              MOVE "APR/UNITPER" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 2
              MOVE "78THS UNIT CHG" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 3
              MOVE "1/12 OF AGREE" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 4
              MOVE "% AT POST" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 5
              MOVE "RECIPROCAL TERM" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 6
              MOVE "LESSER OF 0OR3" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 7
              MOVE "1/12 OF EFFRATE" TO VDUBUF
           ELSE
           IF SP-DEFPERCD = 8
              MOVE "1/TERM++$WORKER" TO VDUBUF
           ELSE
              MOVE "INVALID" TO VDUBUF.
           PERFORM SCREENIO.
       VALIDATE-FORMNO.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           PERFORM CLOSE-FD1-FILE.
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

      *************************************************
       IO-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-FD1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *************************************************
       END-ROUTINE SECTION.
      *************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           PERFORM FORM-RESET.
           DESTROY SPMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
