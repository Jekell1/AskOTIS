       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMAND.
      **************************************************************************
      *   DIR : MN
      *   DESC: PAGE 2 OF REFUNDS - PROG 13
      *
      * REV:
      *  MJD 050895 ADDED CODE TO WRITE CHANGE FILE AFTER REWRITE
      *  JFF 120495 ADDED LOAN CLASS SCAN & STATE PARAMETER SCAN
      *  JFF 070896 ADDED SP-RBUNITPER TO REB TABLE AND READING
      *             BR-FILE TO CHECK BR-UNITPER-FG.
      *  MJD 060397 ADDED "14. OTH 3 INS"
      *  BLV 061198 CHANGE ORIG, CHANGED RECS TO USE SP-SIZE(4513) NOT 4502
      *  BAH 990720 ADDED SP-RBSPOPT2 8, INSURANCE ORGTERM SWING 16 DAYS FOR
      *             RULE 78THS REFUNDING, REGACC # 911
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *  BAH 010413 ADDED SP-RBSPOPT2 9, FULL REFUND UP TO "SAME AS CASH" DATE
      *             LENDMARK, PR# 1443
      *  JTG 010503 ALLOWED SP-RBFRMLA (5) TO WORK WITH SP-RBUNITPER (A)
      *             NEW CODE 'B' FOR SP-RBUNITPER-CD(OCCURS 16) PAACO PR#1466
      *  BAH 010910 ADDED SP-RBSPOPT2 10, FULL REFUND UP TO 60 DAYS, REGACC
      *             PR# 1550
      *  JTG 040917 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS
      *             ADDED AERN-LN-INSEFF & AERN-LN-INSEXP
      *             ALSO ADDED SP-RBSPOPT2 = 11 NEW ROUNDING OPTION TO
      *             TAKE COMPUTED REBATE AND ROUNDED UP OR DOWN TO
      *             WHOLE DOLLARS                                   REGACC #0074
      *   CS 041020 ADD CL/SPMEMO WHICH WRITES SPR MEMO (SMFILE) WHEN A FIELD
      *             IS CHANGED [WORLD PR#362]
      *   CS 060217 ADDED TEST IF USING SP-RBSPOPT1 = 24, MUST BE A 360 YR TYPE
      *                                                        PR#2680 PEOPLE
      *  BAH 100415 ADDED SP-RBSPOPT2 12 FOR REGMGM PR# 3570
      *  CS  100805 ALLOWED ENTRY OF SP-RBUNITPER-CD OF 'B' WITH ACTUARIAL
      *             REBATE 'B' & SP-ORGST = "MX", PREV SP-RBUNITPER-CD WAS
      *             ALLOWED WITH 78'S OR PRORATA REFUNDS WORLD PL#692
      *  CS  100921 JR WENT TO PUT IN A PAYCODE FREQ & GOT A MESSAGE THAT ONLY
      *             SPACE WAS VALID, WE ARE CHECKING BR-UNITPER-FG = "Y" TO
      *             ALLOW AN ENTRY OF PAYCODE FREQ? PER JR, THIS TEST HERE
      *             SINCE 1996 BUT PER JR, DON'T TEST   WORLD PR#692
      *  BAH 111031 ADDED SP-RBSPOPT2 13 FOR COLTN PR# 3900
      *  BAH 121119 ALLOW PAYCODE FREQ "B" FOR INTEREST RBFRMLA "B" FOR
      *             EVERYONE, NOT JUST WORLD "MX" , SECAUTO PR# 4156
      * MJD 160613 ADDED SP-RBDTE = "D"                              71105#0918
      *  CS 160803 SAVEKEY FROM X(8) TO X(9), ORIGINAL-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      * BAH 0517.2023 DISABLED CALIFORNIA IB RECASTING FORMULA 2  KM
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
           "@(#)MN/SPMAND.CBL S35 05/17/23-11:21:40 >".
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
       01  SUB               PIC 99     COMP-3 VALUE 0.
       01  S                 PIC 99     COMP-3.
       01  ELE               PIC 99     VALUE 0.
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
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  REB-TABLE       OCCURS 14.
               05  F-RBDAYS1   PIC ZZ.
               05  F-RBDTE1    PIC X.
               05  F-RBYRTYPE1 PIC ZZZ.
               05  F-RBDAYS2   PIC ZZ.
               05  F-RBDTE2    PIC X.
               05  F-RBYRTYPE2 PIC ZZZ.
               05  F-RBRECAST  PIC ZZ.
               05  F-RBSPOPT2  PIC ZZ.
               05  F-RBMANAGER PIC ZZ.
               05  F-RBUNITPER PIC X.

       01  DISPLAY-LISTS.
           03  FILLER        PIC X(52)  VALUE
           "010 011 012 013 014 015 016 017 018 019 020 021 022 ".
           03  FILLER        PIC X(52)  VALUE
           "023 024 025 026 027 028 029 030 031 032 033 034 035 ".
           03  FILLER        PIC X(52)  VALUE
           "036 037 038 039 040 041 042 043 044 045 046 047 048 ".
           03  FILLER        PIC X(56)  VALUE
           "049 050 051 052 053 054 055 056 057 058 059 060 061 062 ".
           03  FILLER        PIC X(52)  VALUE
           "063 064 065 066 067 068 069 070 071 072 073 074 075 ".
           03  FILLER        PIC X(52)  VALUE
           "076 077 078 079 080 081 082 083 084 085 086 087 088 ".
           03  FILLER        PIC X(56)  VALUE
           "089 090 091 092 093 094 095 096 097 098 099 100 101 102 ".
           03  FILLER        PIC X(52)  VALUE
           "103 104 105 106 107 108 109 110 111 112 113 114 115 ".
           03  FILLER        PIC X(52)  VALUE
           "116 117 118 119 120 121 122 123 124 125 126 127 128 ".
           03  FILLER        PIC X(44)  VALUE
           "129 130 131 132 133 134 135 136 137 138 139 ".
           03  FILLER        PIC X(40)  VALUE
           "140 141 142 143 144 145 146 147 148 149.".
       01  DISPLAY-LIST REDEFINES DISPLAY-LISTS
                             PIC X(560).

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX              PIC 99 COMP-3 VALUE 14.
       01  SPEC-TABLE.
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
           03  FILLER        PIC X(8)   VALUE "ENNN020 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 14 TIMES.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMAND_DEF.CPY".
       COPY "LIBMN/SPMAND_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMAND_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMAND" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.
       MAIN-MODULE.
      D    STOP MESS.
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
              MOVE 14 TO FORM-SEL2.
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

      ********************************************
       REC-SP-MODULE SECTION.
      ********************************************
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

      ********************************************
       CLASS-SCAN SECTION.
      ********************************************
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

      ********************************************
       ENTER-SCANKEY SECTION.
      ********************************************
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
              IF NOT FORM-2-DIGITS OR FORM-SEL2 = 13
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
           IF CHNO = 11
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
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO NEW-REC
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
              DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      ***************************
      *    SET RECORD ELEMENTS
      ***************************
       F-01.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-02.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-03.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-04.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-05.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-06.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-07.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-08.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-09.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-10.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-11.
           GO TO CREATE-ROUTINE.
       F-12.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-13.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       F-14.
           PERFORM ENTER-REBATE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL1 FORM-SEL2.
       EXIT-ENTRY-MODULE.
           EXIT.

      ************************************************
       ENTER-REBATE SECTION.
      ************************************************

      * ADJUST ELE TO SUBSCRIPT TABLE

           SUBTRACT 0 FROM ELE GIVING S.
           MOVE VDUNUM0 TO SP-RBDAYS(S, 1).
       ENTER-RBDTE1.
           MOVE "E  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM ELE
              GO TO ENTER-REBATE-TABLE-EXIT.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBDTE1.
           MOVE VDUBUF TO SP-RBDTE(S, 1).
           IF SP-RBDTE(S, 1) = " "
              IF SP-RBFRMLA(S) NOT = " "
                 MOVE "ENTER: SPACE ONLY IF FORMULA IS A SPACE"
                      TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBDTE1
              ELSE
                 GO TO ENTER-RBYRTYPE1.
           IF NOT SP-RBDTE-VALID(S, 1)
              MOVE "ENTER: 'A', 'B', 'C', OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBDTE1.
           IF SP-RBDTE(S,1) = "D" AND (S = 7 OR 8 OR 9 OR 10 OR 11)
              MOVE "FORMULA 'D' IS ONLY VALID FOR INSURANCE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBDTE1.
       ENTER-RBYRTYPE1.
           MOVE "ENNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBDTE1.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBYRTYPE1.
           MOVE VDUNUM0 TO SP-RBYRTYPE(S, 1).
           IF SP-RBYRTYPE(S, 1) = 0
              IF SP-RBFRMLA(S) NOT = " "
                 MOVE "ENTER: 0 ONLY IF FORMULA IS A SPACE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBYRTYPE1
              ELSE
                 GO TO ENTER-RBDAYS2.
           IF NOT SP-RBYRTYPE-VALID(S, 1)
              MOVE "ENTER: 358, 360, 365, 367" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBYRTYPE1.
           IF SP-RBYRTYPE(S,1) = 365 AND
               SP-RBSPOPT1(S) = 24
                MOVE "SPECIAL OPTION 24 REQUIRES A 360 YEAR TYPE"
                                                    TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBYRTYPE1.

       ENTER-RBDAYS2.
           MOVE "ENNN020  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBYRTYPE1.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBDAYS2.
           MOVE VDUNUM0 TO SP-RBDAYS(S, 2).
       ENTER-RBDTE2.
           MOVE "E  A010  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBDAYS2.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBDTE2.
           MOVE VDUBUF TO SP-RBDTE(S, 2).
           IF SP-RBDTE(S, 2) = " "
              IF SP-RBFRMLA(S) NOT = " "
                 MOVE "ENTER: SPACE ONLY IF FORMULA IS A SPACE"
                      TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBDTE2
              ELSE
                 GO TO ENTER-RBYRTYPE2.
           IF NOT SP-RBDTE-VALID(S, 2)
              MOVE "ENTER: 'A', 'B', 'C', 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBDTE2.
           IF SP-RBDTE(S,2) = "D" AND (S = 7 OR 8 OR 9 OR 10 OR 11)
              MOVE "FORMULA 'D' IS ONLY VALID FOR INSURANCE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBDTE2.
       ENTER-RBYRTYPE2.
           MOVE "ENNN030  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBDTE2.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBYRTYPE2.
           MOVE VDUNUM0 TO SP-RBYRTYPE(S, 2).
           IF SP-RBYRTYPE(S, 2) = 0
              IF SP-RBFRMLA(S) NOT = " "
                 MOVE "ENTER: 0 ONLY IF FORMULA IS A SPACE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBYRTYPE2
              ELSE
                 GO TO ENTER-RBRECAST.
           IF NOT SP-RBYRTYPE-VALID(S, 2)
              MOVE "ENTER: 358, 360, 365, 367" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBYRTYPE2.
           IF SP-RBYRTYPE(S,2) = 365 AND
               SP-RBSPOPT1(S) = 24
                MOVE "SPECIAL OPTION 24 REQUIRES A 360 YEAR TYPE"
                                                    TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-RBYRTYPE2.

       ENTER-RBRECAST.
           MOVE "ENNN020  6." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBYRTYPE2.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBRECAST.
      *    IF NOT (VDUNUM0 = 0 OR 1 OR 2)
           IF NOT (VDUNUM0 = 0 OR 1)
              MOVE "ENTER 0 OR 1" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBRECAST.
      * DISABLED CALIFORNIA IB RECASTING FORMULA 2  5/17/2023
      *    IF VDUNUM2 = 2
      *       IF LC-LOANTYPE NOT = "I"
      *          MOVE "SELECTION 2 IS FOR IB LOAN CLASSES" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO ENTER-RBRECAST.
           MOVE VDUNUM0 TO SP-RBRECAST(S).
       ENTER-RBSPOPT2.
           MOVE "ENNN020  7." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBRECAST.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBSPOPT2.
           MOVE VDUNUM0 TO SP-RBSPOPT2(S).
           IF SP-RBSPOPT2(S) = 1 OR 4 OR 5 OR 8
              IF S = 7 OR 8 OR 9 OR 10
                 MOVE "1, 4, 5 AND 8 ARE FOR INSURANCE ONLY!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT2.
           IF SP-RBSPOPT2(S) = 2 OR 3
              IF S NOT = 7
                 MOVE "OPTION CODE 2 OR 3 FOR INTEREST ONLY!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBSPOPT2.
           IF NOT SP-RBSPOPT2-VALID(S)
              MOVE "ENTER: 0 OR 1 THRU 13" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBSPOPT2.
       ENTER-RBMANAGER.
           MOVE "ENNN020  8." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBSPOPT2.
           IF VDUSTATUS = "1D"
              MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBMANAGER.
           MOVE VDUNUM0 TO SP-RBMANAGER(S).
       ENTER-RBUNITPER.
           MOVE "E  A010  9." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-RBMANAGER.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-RBUNITPER.

      *OPEN PAYCODE FREQ 'B' (ELAPSED TIME IN UNIT PERIODS) FOR WORLD MEXICO
      *SEMI-MONTHLY ACCTS IF THE FOLLOWING IS SET!
      *OPEN PAYCODE FREQ 'B' (ELAPSED TIME IN UNIT PERIODS) FOR EVERYONE!
      * BAH 11/19/2012 JIM R, SEC AUTO PR# 4156

      *    IF SP-ORGST = "MX"
             IF ELE = 7
                IF SP-RBFRMLA(7) = "B"
                    IF VDUBUF = "B"
                       GO TO VERIFY-RBUNITPER.

           IF VDUBUF = "A" OR "B"
              IF NOT (SP-RBFRMLA(S) = "1" OR "3" OR "5" OR "A")
                 MOVE "REBATE FORMULA MUST BE '1' OR '3' OR '5' OR 'A'"
                                                              TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-RBUNITPER.

       VERIFY-RBUNITPER.
           IF NOT (VDUBUF = " " OR "A" OR "B")
              MOVE "ENTER 'SPACE' OR 'A' OR 'B'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-RBUNITPER.

           MOVE VDUBUF TO SP-RBUNITPER-CD(S).

       ENTER-REBATE-TABLE-EXIT.
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
      *CREATE SPR MEMO
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

           MOVE 0 TO SUB.
       NEXT-INS-RATE.
           ADD 1 TO SUB.
           MOVE SP-RBDAYS(SUB, 1) TO F-RBDAYS1(SUB).
           MOVE SP-RBDTE(SUB, 1) TO F-RBDTE1(SUB).
           MOVE SP-RBYRTYPE(SUB, 1) TO F-RBYRTYPE1(SUB).
           MOVE SP-RBDAYS(SUB, 2) TO F-RBDAYS2(SUB).
           MOVE SP-RBDTE(SUB, 2) TO F-RBDTE2(SUB).
           MOVE SP-RBYRTYPE(SUB, 2) TO F-RBYRTYPE2(SUB).
           MOVE SP-RBRECAST(SUB) TO F-RBRECAST(SUB).
           MOVE SP-RBSPOPT2(SUB) TO F-RBSPOPT2(SUB).
           MOVE SP-RBMANAGER(SUB) TO F-RBMANAGER(SUB).
           MOVE SP-RBUNITPER-CD(SUB) TO F-RBUNITPER(SUB).
      *--->IF SUB < 13
           IF SUB < 14
              GO TO NEXT-INS-RATE.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SPMAND-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMAND-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMAND-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMAND-FORM-FIELDS TO WR-BUF.
           MOVE SPMAND-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPMAND-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMAND-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMAND_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

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
           IF IO-FG = 0
              MOVE "S  A160LCDESC." TO VDU
              MOVE LC-DESC TO VDUBUF
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

      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
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

      ************************************************
       END-ROUTINE SECTION.
      ************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMAND-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
