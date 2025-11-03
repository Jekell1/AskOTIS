       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMANB.
       AUTHOR.          KEC.
       DATE-WRITTEN.    091498.
      ******************************************************************
      *
      *  PROGRAM ID : MN/LNMANB.C
      *
      *  DESCRIPTION: LOAN MASTER FILE MAINTENANCE --- PAGE 11
      *
       
      * REV:
      *  CS 160628 ADD LN-NEXIS-CRSCORE
      *  CS 160711 SETUP LN-OWNBR TO BE PART OF KEY
      * BLM 161021 ALLOW SPACE, R, V, OR A IN PARVHELD-PAID-FG
      *  CS 161021 ADD LN-CREDITLIM TO PAGE 11 INCLUDING DECIMALS
      *  CS 170301 ADD LN-CREATION-STATUS
      *  CS 170426 ACTIVATE 8 DIGIT DATES                               
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED ORIGINAL-REC AND CHANGED-REC WHICH NOW USING
      *                  LNOR-REC AND LNCH-REC.
      *
      * 1210817 ADD DISPLAY OF LN-CREATION-STATUS IN DISPLAY FIELDS
      * BAH 20190625 REMOVED LN-ZERO-BAL-CNT AND LN-REVOLVING PT0153
      * BAH 2024.0404 ADDED LN-LOS-APP-ID
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/LNMANB.CBL J35 04/04/24-14:57:58 >".

       
      *
      *    F I L E - P A T H - W O R K E R S
      *
       
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       
      *   LNCH-REC FROM 'REPLACING LEADING' ON LN-REC. [CHANGED-REC]
      *   LNOR-REC FROM 'REPLACING LEADING' ON LN-REC. [ORIGINAL-REC]
       
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNCH".
       COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "LNOR".

       
       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR           PIC 9(4).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-NUMBER       PIC Z(7).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-FIELD        PIC X(3).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-DESC         PIC X(10).
           03  FILLER                   PIC X.
           03  WORK-ERRLOG-LONG-ALP.
               05  WORK-ERRLOG-ORG-ALP  PIC X(12).
               05  WORK-ERRLOG-ORG-N0 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-ORG-N2 REDEFINES
                   WORK-ERRLOG-ORG-ALP  PIC Z(8).99-.

               05  WORK-ERRLOG-TO-X     PIC X(3).

               05  WORK-ERRLOG-CHG-ALP  PIC X(12).
               05  WORK-ERRLOG-CHG-N0 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(11)-.
               05  WORK-ERRLOG-CHG-N2 REDEFINES
                   WORK-ERRLOG-CHG-ALP  PIC Z(8).99-.


       
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
       
       01  ACTION                                    VALUE SPACES.
           03  CHKEY                PIC X(04).
           03  CHNO                 PIC 9(02).
           03  CHNO-X     REDEFINES CHNO
                                    PIC X(02).
       01  FORM-NUM1      REDEFINES ACTION.
           03  FORM-KEY1            PIC X(04).
           03  FORM-P1              PIC X(01).
           03  FORM-SEL1            PIC 9(01).
       01  FORM-NUM2      REDEFINES ACTION.
           03  FORM-KEY2            PIC X(03).
           03  FORM-P2              PIC X(01).
           03  FORM-SEL2            PIC 9(02).
               88  FORM-CUR-PAGE    VALUE 11.
               88  FORM-2-DIGITS    VALUE 10 11.              

       01  FIRST-SCAN               PIC 9(01)        VALUE ZEROES.
       01  FIRST-TIME               PIC 9(01)        VALUE ZEROES.

       01  IO-TYPE                  PIC X(03)        VALUE SPACES.
       01  NEW-FLAG                 PIC 9(01)        VALUE ZEROES.
       01  ONE                      PIC S9(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    'PAGE-CHANGE-FLAG' IS FOR KNOWING IF COMING FROM ANOTHER PAGE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-CHANGE-FLAG         PIC X(01)        VALUE "N".
           88  PAGE-CHANGE-NO                        VALUE "N".
           88  PAGE-CHANGE-YES                       VALUE "Y".

       01  WS-ORGBR                 PIC 9(04)        VALUE ZEROES.

       01  LNMAIN-LINK.
           03  FORM-NAME            PIC X(20).
           03  SAVEKEY              PIC X(08).
           03  NEW-REC              PIC X(01).
           03  ERRCD                PIC X(01).
           03  FORMSEL              PIC 9(02).
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).
       
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
       
       01  DISPLAY-BUF                VALUE SPACES.
           03  F01                    PIC X. 
           03  F02                    PIC Z(6)9.99-   BLANK ZERO.
           03  F03                    PIC Z(6)9.99-   BLANK ZERO.
           03  F04                    PIC X.                         
           03  F05                    PIC 9(8)        BLANK ZERO.   
           03  F06                    PIC X.
           03  F07                    PIC X.
           03  F08                    PIC 9(3)        BLANK ZERO.
           03  F09                    PIC 99/99/99    BLANK ZERO.
           03  F10                    PIC 99/99/99    BLANK ZERO.
           03  F11                    PIC X.
           03  F12                    PIC X.
           03  F13                    PIC 9(3)        BLANK ZERO.
           03  F14                    PIC ZZZ,ZZ9.99.
           03  F15                    PIC X.
           03  F16                    PIC 9(18).
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(28) VALUE "010 020 030 040 050 060 070 ".
           03  FILLER PIC X(28) VALUE "080 090 100 110 120 130 140 ".
           03  FILLER PIC X(8)  VALUE "150 160.".
       
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
       
       01  SPEC-TABLE.
      * 01. PARVHEL PAID FG
           03  FILLER   PIC X(08)   VALUE "E  A010N".
      * 02. IL GROSS INCOME    
           03  FILLER   PIC X(08)   VALUE "ENNS072N".
      * 03. IL GROSS INCOME 2
           03  FILLER   PIC X(08)   VALUE "ENNS072N".
      * 04. GARNISH FG
           03  FILLER   PIC X(08)   VALUE "E  A010N".
      * 05. WCBC EMP ID   
           03  FILLER   PIC X(08)   VALUE "ENNN080N".  
      * 06. 1099-C FLAG
           03  FILLER   PIC X(08)   VALUE "E  A010N".  
      * 07. ESCROW-FLAG
           03  FILLER   PIC X(08)   VALUE "E  A010N".  
      * 08. NEXIS CRSCORE
           03  FILLER   PIC X(08)   VALUE "ENNN030N".  
      * 09. N/A
           03  FILLER   PIC X(08)   VALUE "ERNM060N".
      * 10. N/A
           03  FILLER   PIC X(08)   VALUE "ERNM060N".
      * 11. N/A
           03  FILLER   PIC X(08)   VALUE "E  A010N".
      * 12. N/A
           03  FILLER   PIC X(08)   VALUE "E  A010N".
      * 13. N/A
           03  FILLER   PIC X(08)   VALUE "ENNN030N".
      * 14. CREDIT LIMIT
           03  FILLER   PIC X(08)   VALUE "E NN062 ".
      * 15. CREDIT LIMIT
           03  FILLER   PIC X(08)   VALUE "E  A010N".
      * 16. LOS APP ID
           03  FILLER   PIC X(08)   VALUE "EN N180N".
      *-----------------------------------------------------------------

       01  SPEC-TAB            REDEFINES SPEC-TABLE.
           03  SPEC-REC                  OCCURS 16 TIMES.
               05  SPEC                  PIC X(07).
               05  CHFG                  PIC X(01).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MAX                           PIC 9(02)     VALUE 16.    
       01  ELE                           PIC 9(02)     VALUE ZEROES.
      *    88  ELE-NOT-AVAILABLE                       VALUE 01 THRU 05.

       
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
       
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/LNMANB_DEF.CPY".
       COPY "LIBMN/LNMANB_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

       
      *    L I N K A G E - S E C T I O N
       
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/LNMANB_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNMANB" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

       MAIN-MODULE.

           MOVE SPACES TO VDU-PGMENU-SELECTION.
           PERFORM FORM-RESET.
           IF ( FIRST-TIME = 0 )
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-LN-MODULE.
           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO EXIT-MAIN-PROGRAM.
           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.
           IF ( NEW-REC = "Y" )
              MOVE "N"    TO NEW-REC
              MOVE "P"    TO FORM-P1
              MOVE 1      TO FORM-SEL1
              MOVE SPACES TO SAVEKEY.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE LN-ACCTNO TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           IF ( FORM-SEL2 NOT = ZEROES ) AND
              ( FORM-P2       = "P"    )
              MOVE LN-ACCTNO TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.

           GO TO END-PROGRAM.


      ******************************************************************
       REC-LN-MODULE SECTION.
      ******************************************************************
       REC-LN-MODULE-NO.

           MOVE 0 TO LN-ACCTNO LN-OWNBR FIRST-SCAN.
           MOVE SPACES TO IO-TYPE.

           IF ( SAVEKEY NOT = SPACES )
              MOVE "Y"     TO PAGE-CHANGE-FLAG
              MOVE SAVEKEY TO LN-ACCTNO
              MOVE SPACES  TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-LNFILE.

           MOVE "N"    TO PAGE-CHANGE-FLAG.
           MOVE ZEROES TO NEW-FLAG.

       REC-LN-ACCTNO.

           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO EXIT-REC-LN-MODULE.
           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              GO TO EXIT-REC-LN-MODULE.
           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-LN-ACCTNO.

           MOVE VDUNUM0 TO LN-ACCTNO.

           IF ( LN-ACCTNO = ZEROES )
              GO TO REC-LN-ACCTNO.

      *-----------------------------------------------------------------
       READ-LNFILE.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF ( IO-FG NOT = ZEROES )
              MOVE "* NOT ON FILE *" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO REC-LN-MODULE-NO
           ELSE
              MOVE LN-REC TO LNOR-REC
              PERFORM DISPLAY-FIELDS
              MOVE ZEROES TO NEW-FLAG
              IF ( NEW-REC = "Y" )
                 MOVE 1 TO NEW-FLAG
              END-IF
              IF ( LN-POCD-INRESCISSION ) AND ( PAGE-CHANGE-NO )
                 MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                 PERFORM SEND-MESS
              END-IF.

       EXIT-REC-LN-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE " ENTER LOAN #, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.

       ENTER-KEY.

           MOVE "EN N080SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-SCANKEY.
           IF ( VDUSTATUS = "1<" )
              GO TO READ-FILES-NEXT.
           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-KEY.

           IF ( VDUNUM0 = ZEROES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE LN-REC TO LNOR-REC
                 IF ( LN-POCD-INRESCISSION )
                    MOVE "ACCOUNT IS CURRENTLY IN RESCISSION" TO MESS
                    PERFORM SEND-MESS
                 END-IF
                 GO TO END-ENTER-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0       TO LN-ACCTNO.
           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           PERFORM START-LN1-FILE.

       READ-FILES-NEXT.

           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-SCANKEY.

           IF ( LN-POCD-VOID ) OR ( LN-POCD-RESCIND )
              GO TO READ-FILES-NEXT.

      *    UNLOCK LN-FILE.
           PERFORM DISPLAY-SCAN-KEY.

           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.

           MOVE "SN N060LNNO." TO VDU.
           MOVE LN-ACCTNO      TO VDUNUM0.
           PERFORM SCREENIO.


       END-ENTER-SCANKEY.

           MOVE ZEROES TO FIRST-SCAN.
           PERFORM CLOSE-LN1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

      *-----------------------------------------------------------------
       EXIT-ENTER-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE ZEROES TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE     < 1       ) MOVE 1 TO ELE ONE.
           IF ( ELE NOT > MAX     ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P"    ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 < 1 ) OR ( FORM-SEL1 > 9 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FORM-NUM2 CHECKS FOR PAGES 10 THRU 99
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FORM-KEY2 = SPACES ) AND
              ( FORM-P2   = "P"    ) AND
              ( FORM-SEL2 NUMERIC  )
              IF ( FORM-CUR-PAGE  ) OR ( NOT FORM-2-DIGITS )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

           GO TO CHECK-CHNO.

      *-----------------------------------------------------------------
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.

      *-----------------------------------------------------------------
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO-X NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = ZEROES )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY    NOT = ZEROES ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( NEW-FLAG = ZEROES ) AND
              ( CHFG(ELE) = "N"   )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = EXT-CONAME-DAILY-PASSWORD )
                 GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
      *    ENTER ON SELECTED FIELD.
      *-----------------------------------------------------------------
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F1-KEY.

           IF F1-KEY 
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

       TEST-DOWN-KEY.

           IF DOWN-KEY 
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-UP-KEY.

           IF UP-KEY 
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE.

       TEST-ENTER-KEY.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.

           MOVE VDUBUF TO LN-PARVHELD-PAID-FG.           
           IF NOT (LN-PARVHELD-PAID-FG  = " " OR "A" OR "R" OR "V")
              MOVE "ENTER ' ' OR 'N' OR 'R' OR 'A' ONLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO LN-IL-GROSS-INCOME.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO LN-IL-GROSS-INCOME2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO LN-GARNISH-EXTR.               
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
      *WCBC EMPLOYEE ID
           MOVE VDUNUM0  TO LN-EMP-ID.              
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF   TO LN-1099-C-FG.           
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO LN-ESCROW-FG.
           IF LN-ESCROW-FG NOT = "Y"
              IF LN-ESCROW-FG NOT = "N"
                 IF LN-ESCROW-FG NOT = "I"
                    MOVE "PLEASE ENTER 'Y' OR 'N' OR 'I'" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
      
      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM0  TO LN-NEXIS-CRSCORE.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-11.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-12.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO LN-CREDITLIM.
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-15.
           MOVE VDUBUF   TO LN-CREATION-STATUS.           
           IF NOT(LN-CREATION-EXTERNAL OR LN-CREATION-INTERNAL)
              MOVE "INVALID: 'I', 'L' OR SPACE ONLY " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       
       F-16.
           MOVE VDUNUM0   TO LN-LOS-APP-ID.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE ZEROES TO FORM-SEL1 FORM-SEL2.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-LN1-FILE.

           MOVE LN-REC TO LNCH-REC.

           IF ( LNOR-REC = LNCH-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-LN1-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( LN-REC NOT = LNOR-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE LNCH-REC TO LN-REC.

           PERFORM REWRITE-LN1-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ****************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ****************************************************
           MOVE SPACES    TO WORK-ERRLOG-DETAIL.
           MOVE LN-OWNBR  TO WORK-ERRLOG-BR.
           MOVE LN-ACCTNO TO WORK-ERRLOG-NUMBER.
           MOVE "TO "     TO  WORK-ERRLOG-TO-X.

           IF LNOR-PARVHELD-PAID-FG NOT = LNCH-PARVHELD-PAID-FG
              MOVE "01 "                  TO WORK-ERRLOG-FIELD 
              MOVE "PARTICH PD"           TO WORK-ERRLOG-DESC
              MOVE LNOR-PARVHELD-PAID-FG  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-PARVHELD-PAID-FG  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-IL-GROSS-INCOME NOT = LNCH-IL-GROSS-INCOME 
              MOVE "02 "                  TO WORK-ERRLOG-FIELD 
              MOVE "IL INCOME "           TO WORK-ERRLOG-DESC
              MOVE LNOR-IL-GROSS-INCOME  TO WORK-ERRLOG-ORG-N2    
              MOVE LNCH-IL-GROSS-INCOME  TO WORK-ERRLOG-CHG-N2
              PERFORM LOG-FIELD-CHANGE.


           IF LNOR-IL-GROSS-INCOME2 NOT = LNCH-IL-GROSS-INCOME2
              MOVE "03 "                  TO WORK-ERRLOG-FIELD 
              MOVE "IL INCOME2"           TO WORK-ERRLOG-DESC
              MOVE LNOR-IL-GROSS-INCOME2  TO WORK-ERRLOG-ORG-N2 
              MOVE LNCH-IL-GROSS-INCOME2  TO WORK-ERRLOG-CHG-N2
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-GARNISH-EXTR NOT = LNCH-GARNISH-EXTR
              MOVE "04 "                  TO WORK-ERRLOG-FIELD 
              MOVE "GARN EXTR "             TO WORK-ERRLOG-DESC
              MOVE LNOR-GARNISH-EXTR  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-GARNISH-EXTR  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-EMP-ID NOT = LNCH-EMP-ID
              MOVE "05 "                  TO WORK-ERRLOG-FIELD 
              MOVE "WCBC EMPID"             TO WORK-ERRLOG-DESC
              MOVE LNOR-EMP-ID  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-EMP-ID  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-1099-C-FG NOT = LNCH-1099-C-FG
              MOVE "06 "                  TO WORK-ERRLOG-FIELD 
              MOVE "1099-C FLG"           TO WORK-ERRLOG-DESC
              MOVE LNOR-1099-C-FG  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-1099-C-FG  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-ESCROW-FG NOT = LNCH-ESCROW-FG
              MOVE "07 "                  TO WORK-ERRLOG-FIELD 
              MOVE "ESCROW FLG"           TO WORK-ERRLOG-DESC
              MOVE LNOR-ESCROW-FG  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-ESCROW-FG  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-NEXIS-CRSCORE NOT = LNCH-NEXIS-CRSCORE
              MOVE "08 "                  TO WORK-ERRLOG-FIELD 
              MOVE "NEXIS CRSC"           TO WORK-ERRLOG-DESC
              MOVE LNOR-NEXIS-CRSCORE  TO WORK-ERRLOG-ORG-ALP
              MOVE LNCH-NEXIS-CRSCORE  TO WORK-ERRLOG-CHG-ALP
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CREDITLIM NOT = LNCH-CREDITLIM
              MOVE "14 "                  TO WORK-ERRLOG-FIELD 
              MOVE "CREDITLIM "           TO WORK-ERRLOG-DESC
              MOVE LNOR-CREDITLIM  TO WORK-ERRLOG-ORG-N0
              MOVE LNCH-CREDITLIM  TO WORK-ERRLOG-CHG-N0
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-CREATION-STATUS NOT = LNCH-CREATION-STATUS
              MOVE "15 "                  TO WORK-ERRLOG-FIELD 
              MOVE "CREATION  "           TO WORK-ERRLOG-DESC
              MOVE LNOR-CREATION-STATUS  TO WORK-ERRLOG-ORG-N0
              MOVE LNCH-CREATION-STATUS  TO WORK-ERRLOG-CHG-N0
              PERFORM LOG-FIELD-CHANGE.

           IF LNOR-LOS-APP-ID NOT = LNCH-LOS-APP-ID
              MOVE "16 "                  TO WORK-ERRLOG-FIELD 
              MOVE "LOS APP ID "           TO WORK-ERRLOG-DESC
              MOVE LNOR-LOS-APP-ID  TO WORK-ERRLOG-ORG-N0
              MOVE LNCH-LOS-APP-ID  TO WORK-ERRLOG-CHG-N0
              PERFORM LOG-FIELD-CHANGE.

      ****************************************************
       LOG-FIELD-CHANGE SECTION.
      ****************************************************
      * CODE TO WRITE CHANGE FILE.

           MOVE "F" TO ERRLOGW-ACTION.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      ********************************************************
       DISPLAY-FIELDS SECTION.
      ********************************************************

           MOVE SPACES                    TO DISPLAY-BUF.
           MOVE LN-PARVHELD-PAID-FG       TO F01.

           MOVE LN-IL-GROSS-INCOME        TO F02.
           MOVE LN-IL-GROSS-INCOME2       TO F03.
           MOVE LN-GARNISH-EXTR           TO F04.
           MOVE LN-EMP-ID                 TO F05.
           MOVE LN-1099-C-FG              TO F06.
           MOVE LN-ESCROW-FG              TO F07.
           MOVE LN-NEXIS-CRSCORE          TO F08.
           MOVE 0                         TO F09.
           MOVE 0                         TO F10.
           MOVE SPACES                    TO F11.
           MOVE SPACES                    TO F12.
           MOVE 0                         TO F13.
           MOVE LN-CREDITLIM              TO F14.
           MOVE LN-CREATION-STATUS        TO F15.
           MOVE LN-LOS-APP-ID             TO F16.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       EXIT-DISPLAY-FIELDS.
           EXIT.


      **********************************************
       FORM-RESET SECTION.
      **********************************************
           MOVE EXT-CONAME-CONAME TO LNMANB-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNMANB-SYSDATE.
           DISPLAY LNMANB-SCREEN UPON LNMAIN-WINDOW-HANDLE.
           MOVE LNMANB-FORM-FIELDS TO WR-BUF.
           MOVE LNMANB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE LNMANB-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNMANB-HELP--DIR TO HELP-LINK-DIR.

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/LNMANB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LN1-FILE.
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNMANB-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE LNMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
