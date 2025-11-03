       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GRMAIN.
      ******************************************************************
      *
      *                 (MN)
      *
      *  DESCRIPTION:   BRANCH GROUP FILE MAINTENANCE
      *
      *   ***NOTE***:   IF ADDING/DELETING A LEVEL [ELE], ONLY HAVE TO
      *                 ALTER THE FOLLOWING FOR THE 'MEMBER' SECTION:
      *
      *                   88  ELE-MEMBER-ENTRY   VALUE 11.
      *                                                ^^
      *                                                ||-- ***
      *
      *-----------------------------------------------------------------
      * REV:
      * GLF 102497 REMOVED RESET OF FIELDS 2 AND 10
      * BAH 140911 FIXED PHONE ENTRY
      * KEC 2016.0311 CHANGED CD-LV???? TO CD-LV-????
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2018.0201 WAS WORKING ON WHY WHEN EXITING THIS PROGRAM IT WAS
      *          CAUSING LONG BLINKING FLICKER OF THE CURSOR IN THE
      *          UPPER-LEFT CORNER.  FOUND THAT WHEN EXITING THE PROGRAM
      *          WHEN THE CURSOR IS ON THE ACCEPT-YN FIELD WAS CAUSING
      *          THE PROBLEM.  FOUND THAT YOU CANNOT EXIT WHEN THE 
      *          CURSOR IS ON A FIELD IN ROW 23 COLUMN 79 & 80.  ALSO
      *          FOUND YOU CAN HAVE THE FIELD IN ROW 23 COLUMN 79 & 80
      *          BUT NEED TO ADD PERFORM FORM-RESET BEFORE
      *          DOING DESTROY XXXXXX-SCREEN.
      *          MORE INFO CAN BE FOUND IN THE SHARED DRIVE IN DOCUMENT
      *          A30_DISPLAY_SCREEN_ISSUE.DOCX
      * BAH 190130 SPLIT CDFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/GRMAIN S35 04/25/25-08:17:59 >".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".


      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  ACTION.
           03  CHKEY                    PIC X(04).
           03  CHNO                     PIC 9(02).

       01  CHAN                         PIC X(04)    VALUE "CHAN".

       01  ELE                          PIC 9(02).
           88  ELE-MEMBER-ENTRY                      VALUE 11.

       01  ERRCD                        PIC X(01)    VALUE SPACES.
       01  HOLD-KEY                     PIC X(04).
       01  IO-TYPE                      PIC X(03).

       01  LEGEND                       PIC X(73).
       01  LEGEND-LIST                  PIC X(07)    VALUE "LEGEND.".

       01  MEMBER-SPEC.
           03  MEMBER-ACT               PIC X(01)    VALUE "E".
           03  FILLER                   PIC X(10)    VALUE "  A040990-".
           03  MEMBER-NUM               PIC 9(03)    VALUE ZEROES.
           03  FILLER                   PIC X(01)    VALUE ".".

       01  NEW-FLAG                     PIC 9(01).
       01  ONE                          PIC S9(1).

       01  RECORD-SIZES.
           03  CHANGED-REC              PIC X(GR-SIZE) VALUE SPACES.
           03  ORIGINAL-REC             PIC X(GR-SIZE) VALUE SPACES.

       01  SUBSCRIPTS.
           03  SUB                      PIC 9(03)    VALUE ZEROES.
           03  SUB2                     PIC 9(03)    VALUE ZEROES.
           03  TNUM                     PIC 9(06)    VALUE ZEROES.

      *=================================================================
      *
      *    L E G E N D - E L E M E N T S
      *
      *=================================================================
       01  LEGEND-ELEMENTS.

           03  LEGEND-ELEMENT-010.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP NAME       ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-020.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, F6-SCAN GROUP LEVEL NUM".
               05  FILLER               PIC X(33)    VALUE
                   "BERS, ENTER GROUP LEVEL         :".

           03  LEGEND-ELEMENT-030.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER CONTACT NAME     ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-040.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S ADDRESS L".
               05  FILLER               PIC X(33)    VALUE
                   "INE #1                          :".

           03  LEGEND-ELEMENT-050.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S ADDRESS L".
               05  FILLER               PIC X(33)    VALUE
                   "INE #2                          :".

           03  LEGEND-ELEMENT-060.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S CITY     ".
               05  FILLER               PIC X(33)    VALUE
                   "                                :".

           03  LEGEND-ELEMENT-070.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S STATE/ZIP".
               05  FILLER               PIC X(33)    VALUE
                   "CODE                            :".

           03  LEGEND-ELEMENT-080.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S PHONE NUM".
               05  FILLER               PIC X(33)    VALUE
                   "BER                             :".

           03  LEGEND-ELEMENT-090.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S HOME BRAN".
               05  FILLER               PIC X(33)    VALUE
                   "CH NUMBER                       :".

           03  LEGEND-ELEMENT-100.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, ENTER GROUP'S LOAN APPR".
               05  FILLER               PIC X(33)    VALUE
                   "OVAL LIMIT                      :".

           03  LEGEND-ELEMENT-990.
               05  FILLER               PIC X(40)    VALUE
                   "F1-CLEAR SCREEN, F3-CLEAR, F4-DELETE, F5".
               05  FILLER               PIC X(33)    VALUE
                   "-INSERT, F7-DONE W/ MEMBER ENTRY:".

      *-----------------------------------------------------------------
       01  LEGEND-ELEMENTS-TABLE        REDEFINES LEGEND-ELEMENTS.
           03  LEGEND-ELE               PIC X(73)    OCCURS 11 TIMES.

      *-----------------------------------------------------------------
       01  LEGEND-ELEMENT-GRID.
           05  FILLER               PIC X(40)    VALUE
               "F6-SCAN GROUP ID'S, F7-EXIT, ENTER GROUP".
           05  FILLER               PIC X(33)    VALUE
               " IDENTIFICATION NAME            :".

       01  LEGEND-ELEMENT-SEL.
           05  FILLER               PIC X(40)    VALUE
               "F1-CLEAR SCREEN, 'DELETE', ENTER FIELD N".
           05  FILLER               PIC X(33)    VALUE
               "UMBER                           :".

      *=================================================================
      *
      *    D I S P L A Y - B U F F E R S
      *
      *-----------------------------------------------------------------
      *    SCREEN DISPLAY BUFFER
      *=================================================================
       01  DISPLAY-BUF                                     VALUE SPACES.
           03  F01                         PIC X(20).
           03  F02                         PIC 9(01).
           03  F021                        PIC X(20).
           03  F03                         PIC X(20).
           03  F04                         PIC X(20).
           03  F05                         PIC X(20).
           03  F06                         PIC X(15).
           03  F07                         PIC X(02).
           03  F071                        PIC X(10).
           03  F08                         PIC X(14).
           03  F09                         PIC 9(04).
           03  F10                         PIC Z,ZZZ,ZZ9.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER                      PIC X(48)       VALUE
               "010 020 021 030 040 050 060 070 071 080 090 100.".

      *-----------------------------------------------------------------
      *
      *    DISPLAY-99-BUF --- DISPLAY MEMBERS
      *
      *-----------------------------------------------------------------
       01  DISPLAY-99-BUF.
           03  F99                         PIC X(04)       OCCURS 100.

      *-----------------------------------------------------------------
       01  DISPLAY-99-LIST.
           03  FILLER                      PIC X(48)  VALUE
               "990-001 990-002 990-003 990-004 990-005 990-006 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-007 990-008 990-009 990-010 990-011 990-012 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-013 990-014 990-015 990-016 990-017 990-018 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-019 990-020 990-021 990-022 990-023 990-024 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-025 990-026 990-027 990-028 990-029 990-030 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-031 990-032 990-033 990-034 990-035 990-036 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-037 990-038 990-039 990-040 990-041 990-042 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-043 990-044 990-045 990-046 990-047 990-048 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-049 990-050 990-051 990-052 990-053 990-054 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-055 990-056 990-057 990-058 990-059 990-060 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-061 990-062 990-063 990-064 990-065 990-066 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-067 990-068 990-069 990-070 990-071 990-072 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-073 990-074 990-075 990-076 990-077 990-078 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-079 990-080 990-081 990-082 990-083 990-084 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-085 990-086 990-087 990-088 990-089 990-090 ".
           03  FILLER                      PIC X(48)  VALUE
               "990-091 990-092 990-093 990-094 990-095 990-096 ".
           03  FILLER                      PIC X(32)  VALUE
               "990-097 990-098 990-099 990-100.".

      *=================================================================
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      *=================================================================
       01  MAX                          PIC 9(02)     VALUE 11.
       01  SPEC-TABLE.
      * 1. GROUP NAME
           03  FILLER                   PIC X(08)     VALUE "E  A200 ".
      * 2. GROUP LEVEL
           03  FILLER                   PIC X(08)     VALUE "ENNN010 ".
      * 3. CONTACT NAME
           03  FILLER                   PIC X(08)     VALUE "E  A200 ".
      * 4. ADDRESS LINE 1
           03  FILLER                   PIC X(08)     VALUE "E  A200 ".
      * 5. ADDRESS LINE 2
           03  FILLER                   PIC X(08)     VALUE "E  A200 ".
      * 6. CITY
           03  FILLER                   PIC X(08)     VALUE "E  A150 ".
      * 7. STATE / ZIP
           03  FILLER                   PIC X(08)     VALUE "E  A020 ".
      * 8. PHONE
           03  FILLER                   PIC X(08)     VALUE "R  A100 ".
      * 9. HOME BRANCH
           03  FILLER                   PIC X(08)     VALUE "ENNN040 ".
      *10. APPROVAL LIMIT
           03  FILLER                   PIC X(08)     VALUE "E NN070 ".
      *11[99]. GROUP MEMBERS
           03  FILLER                   PIC X(08)     VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TABLE2        REDEFINES SPEC-TABLE.
           03  SPEC-REC                 OCCURS 11 TIMES.
               05  SPEC                 PIC X(07).
               05  CHFG                 PIC X(01).

       01  GRMAIN-WINDOW-HANDLE  PIC X(10).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/GRMAIN_DEF.CPY".
       COPY "LIBMN/GRMAIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBMN/GRMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "GRMAIN" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GRMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

       MAIN-MODULE.
      D    STOP MESS.
           PERFORM OPEN-GR1-FILE.

           PERFORM FORM-RESET.

           PERFORM REC-GROUP-MODULE.
           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
              PERFORM DELETE-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-GROUP-MODULE SECTION.
      ******************************************************************
       REC-GROUP-MODULE-NO.

           MOVE SPACES TO DISPLAY-99-BUF.
           MOVE ZEROES TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

           MOVE LEGEND-ELEMENT-GRID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040GRID." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO REC-GROUP-MODULE-NO
              ELSE
                 MOVE "S  A000GRID." TO VDU
                 MOVE GRSCAN-KEY     TO VDUBUF GR1-KEY HOLD-KEY
                 PERFORM SCREENIO
                 GO TO READ-GROUP-FILE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GROUP-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO REC-GROUP-MODULE-NO.

           IF ( VDUBUF = SPACES )
              GO TO REC-GROUP-MODULE-NO.

           MOVE VDUBUF TO GR-ID HOLD-KEY.

      *-----------------------------------------------------------------
       READ-GROUP-FILE.

           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.

           IF ( IO-FG = ZEROES )
              MOVE GR-REC TO ORIGINAL-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-GROUP-MODULE.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1 )
              MOVE 1 TO ELE ONE.

           IF ( ELE NOT > MAX )
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE LEGEND-ELEMENT-SEL TO LEGEND.
           PERFORM SEND-LEGEND.

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

           IF ( ACTION = "DELETE" )
              MOVE "DEL" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO = ZEROES )
              GO TO MODIFY-ROUTINE.

           IF ( CHNO > MAX )
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.

           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG (ELE) = SPACES )
                 IF ( NEW-FLAG NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
           ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE LEGEND-ELE(ELE) TO LEGEND.
           PERFORM SEND-LEGEND.

      * GO TO 'MEMBER' ENTRY SECTION; NEED TO CHANGE
      * 'ELE-MEMBER-ENTRY' IF ADDING A NEW ELEMENT.

           IF ( ELE-MEMBER-ENTRY )
              GO TO F-99.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      * 'F1' KEY ROUTINE: CLEAR SCREEN

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         GO TO END-ENTRY-MODULE.

      * 'F6' KEY ROUTINE: CODE SCAN FOR GROUP LEVEL

           IF ( F6-KEY )
              IF ( ELE = 2 )
                 PERFORM CODE-SCAN
                 IF ( IO-FG = ZEROES )
                    GO TO GOTO-FIELD
                 ELSE
                    GO TO ENTER-ELE.

      * 'UP' KEY ROUTINE:

           IF ( UP-KEY )
              IF ( NEW-FLAG = 1 )
                 MOVE -1 TO ONE
                 GO TO GOTO-FIELD.

      * 'DOWN' KEY ROUTINE:

           IF ( DOWN-KEY ) MOVE 1 TO ONE
                           GO TO GOTO-FIELD.


           IF ( NOT ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-99
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO GR-NAME.
           GO TO CREATE-ROUTINE.

       F-02.
           MOVE VDUNUM0 TO GR-LEVEL.
           PERFORM GET-CD-LV.

           IF ( IO-FG NOT = ZEROES )
              MOVE "NOT A VALID LEVEL!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "S  A150021." TO VDU.
           MOVE CD-DESC       TO VDUBUF
                                 F021.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO GR-CONTACT.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUBUF TO GR-ADDR-1.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO GR-ADDR-2.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO GR-CITY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO GR-STATE.

           MOVE "E  A100071." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-ELE.

           MOVE VDUBUF TO GR-ZIP.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           PERFORM ENTER-PHONE.

           IF ( TEL-BUFX = SPACES )
              MOVE 0 TO TEL-BUF.

           IF ( TEL-BUFX NOT NUMERIC )
              GO TO ENTER-ELE.

           MOVE TEL-BUF TO GR-PHONE.
           MOVE "(   )    -    " TO TEL-EDIT.
           PERFORM SEND-PHONE.
           MOVE "S  A140080." TO VDU.
           MOVE TEL-EDIT TO VDUBUF.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO GR-HOME-BRANCH.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM0 TO GR-LN-APPROVAL-LIMIT.
           GO TO CREATE-ROUTINE.

      *=================================================================
      *
      *    M E M B E R - E N T R Y
      *
      *-----------------------------------------------------------------
      *    ***NOTE*** DON'T HAVE TO CHANGE ANYTHING IN HERE IF ADDING
      *               OR DELETEING A FIELD (ELE)
      *=================================================================
       F-99.

           MOVE ZEROES TO TNUM.

      *-----------------------------------------------------------------
       F-99A.
           ADD 1 TO TNUM.

           IF ( TNUM > 100 )
              MOVE 1 TO TNUM.

           MOVE "E"         TO MEMBER-ACT.
           MOVE TNUM        TO MEMBER-NUM.
           MOVE MEMBER-SPEC TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         GO TO END-ENTRY-MODULE.

           IF ( F3-KEY ) MOVE SPACES TO VDUBUF GR-MEMBER-ID(TNUM)
                         MOVE "00" TO VDUSTATUS.

           IF ( F4-KEY ) PERFORM F99-DELETE
                         SUBTRACT 1 FROM TNUM
                         GO TO F-99A.

           IF ( F5-KEY ) PERFORM F99-INSERT
                         SUBTRACT 1 FROM TNUM
                         GO TO F-99A.

           IF ( UP-KEY ) AND ( TNUM > 1 )
              SUBTRACT 2 FROM TNUM
              GO TO F-99A.

           IF ( F7-KEY )
              MOVE SPACES TO LEGEND
              PERFORM SEND-DISPLAY-99
              PERFORM SEND-LEGEND
              GO TO CREATE-ROUTINE.

           IF NOT ( DOWN-KEY OR ENTER-KEY )
              SUBTRACT 1 FROM TNUM
              GO TO F-99A.

           IF ( VDUBUF = SPACES )
              MOVE GR-MEMBER-LOC(TNUM) TO VDUBUF.

           MOVE VDUBUF TO WGR-WORD.

           IF ( WGR-WORD = SPACES )
              GO TO SKIP-GR-CHECK.

      * NOTE: WGR-MIN-LEVEL = 10 MEANS NUMERIC ONLY

           COMPUTE WGR-MIN-LEVEL = GR-LEVEL + 1.

           MOVE GR-REC TO CHANGED-REC.
           PERFORM CHECK-MEMBER.
           MOVE CHANGED-REC TO GR-REC.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              SUBTRACT 1 FROM TNUM
              GO TO F-99A.

       SKIP-GR-CHECK.

           MOVE WGR-WORD    TO F99(TNUM) GR-MEMBER-ID(TNUM).
           MOVE "S"         TO MEMBER-ACT.

           MOVE MEMBER-SPEC TO VDU.
           MOVE F99(TNUM)   TO VDUBUF.
           PERFORM SCREENIO.

           GO TO F-99A.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "LV"       TO CDSCAN-TYPE.
           MOVE ZEROES     TO CDSCAN-LV-LEVEL.
           MOVE SPACES     TO CDSCAN-LV-FILLER.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT  = "00") OR (CDSCAN-LV-LEVEL = ZEROES)
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "SNNN010020." TO VDU.
           MOVE CDSCAN-LV-LEVEL  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE CDSCAN-LV-LEVEL TO VDUNUM0.
           MOVE ZEROES       TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
       F99-DELETE SECTION.
      ******************************************************************

           MOVE TNUM TO SUB SUB2.

       LOOP-F99-DELETE.

           ADD 1 TO SUB2.

           IF ( SUB2 > 100 )
              GO TO END-F99-DELETE.

           MOVE F99(SUB2)          TO F99(SUB).
           MOVE GR-MEMBER-ID(SUB2) TO GR-MEMBER-ID(SUB).
           ADD  1 TO SUB.

           GO TO LOOP-F99-DELETE.

       END-F99-DELETE.
           MOVE SPACES TO F99(100) GR-MEMBER-ID(100).
           PERFORM SEND-DISPLAY-99.

       EXIT-F99-DELETE.
           EXIT.

      ******************************************************************
       F99-INSERT SECTION.
      ******************************************************************

           MOVE 100 TO SUB SUB2.

       LOOP-F99-INSERT.

           SUBTRACT 1 FROM SUB2.

           IF ( SUB2 < TNUM )
              GO TO END-F99-INSERT.

           MOVE F99(SUB2)          TO F99(SUB).
           MOVE GR-MEMBER-ID(SUB2) TO GR-MEMBER-ID(SUB).
           SUBTRACT 1 FROM SUB.

           GO TO LOOP-F99-INSERT.

       END-F99-INSERT.
           MOVE SPACES TO F99(TNUM) GR-MEMBER-ID(TNUM).
           PERFORM SEND-DISPLAY-99.

       EXIT-F99-INSERT.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-GR1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GR-REC TO CHANGED-REC.

           IF ORIGINAL-REC = CHANGED-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GR-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-REC TO GR-REC.
           PERFORM REWRITE-GR1-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-GR1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GR1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF GR-REC NOT = ORIGINAL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-GR1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-GR1-FILE.

       EXIT-DELETE-MODULE.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1                    TO NEW-FLAG.

           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-GR-FILE.

           MOVE HOLD-KEY TO GR-ID.

           MOVE 1        TO SUB.
           MOVE ZEROES   TO ELE.

       EXIT-NEW-RECORD.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE GR-NAME                 TO F01.
           MOVE GR-LEVEL                TO F02.
           PERFORM GET-CD-LV.

           IF ( IO-FG NOT = ZEROES )
              MOVE "NOT A VALID LEVEL!" TO CD-DESC.

           MOVE CD-DESC                 TO F021.
           MOVE GR-CONTACT              TO F03.
           MOVE GR-ADDR-1               TO F04.
           MOVE GR-ADDR-2               TO F05.
           MOVE GR-CITY                 TO F06.
           MOVE GR-STATE                TO F07.
           MOVE GR-ZIP                  TO F071.

           MOVE GR-PHONE                TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT                TO F08.

           MOVE GR-HOME-BRANCH          TO F09.
           MOVE GR-LN-APPROVAL-LIMIT    TO F10.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 100
                   MOVE GR-MEMBER-ID(SUB) TO F99(SUB)
           END-PERFORM.

       END-DISPLAY-FIELDS.

           PERFORM SEND-DISPLAY.
           PERFORM SEND-DISPLAY-99.

       EXIT-DISPLAY-FIELDS.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GBGRCL.CPY".
       COPY "LIBGB/DATER.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO GRMAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GRMAIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GRMAIN-SCREEN UPON GRMAIN-WINDOW-HANDLE.
           MOVE GRMAIN-FORM-FIELDS TO WR-BUF.
           MOVE GRMAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GRMAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRMAIN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/GRMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD-LV.
           MOVE "LV"     TO CD-TYPE.
           MOVE GR-LEVEL TO CD-LV-LEVEL.
           MOVE SPACES   TO CD-LV-FILLER.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-DISPLAY-99.
           MOVE DISPLAY-99-BUF TO WR-BUF.
           MOVE DISPLAY-99-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-YN.
           MOVE " PLEASE ENTER 'Y' OR 'N'!" TO MESS.
           PERFORM SEND-MESS.
       INVALID-TYPE.
           MOVE "GROUP NOT ON FILE, RE-ENTER!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1IN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
           PERFORM FORM-RESET.
           DESTROY GRMAIN-SCREEN.
           DESTROY GRMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
