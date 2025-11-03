      * COPYMEMBER: LIBLA/LA01MA
      ******************************************************************
      *
      *  MEMBER NAME: LIBLA/LA01MA                              RSZ=225
      *
      *  DESCRIPTION: MASTER APPLICATION FILE
      *
      *  NOTES      : 1.  A MASTER APP RECORD MUST EXIST TO HAVE A
      *                    VALID APPLICATION.
      *      
      *               2.  IT IS OPTIONAL TO HAVE BA & LA RECORDS.
      *      
      *               3.  MA-APPDATE AND MA-STATUS-DATE ARE IN A
      *                    YYYMMDD FORMAT!!!!!
      *
      *-----------------------------------------------------------------
      *  REV:
      *  KEC 011697 REVISED ENTIRE MA-RECORD.
      *  KEC 022197 ADDED MA-CREDIT-SCORE.
      *  JTG 032497 ADDED MA-STATUS    'U' TURNED US DOWN
      *  JTG 032797 CHANGED MA-REFUSAL-CD TO MA-STATUS-CD
      *                "    MA-APP-AMOUNT TO MA-APP-AMOUNT
      *  KEC 052297 ADDED EXTRA FILLER (21) RSZ=150
      *  KEC 021998 ADDED MA-ADDRESS-INFO SEGMENT
      *              [ADR1/ADR2/CITY/STATE/ZIP]  RSZ=225
      *  MJD 990127 ----- UPDATED IN FLEXGEN -----
      *  KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *
      *  KEC 1999.0719 ADDED THE FOLLOWING FIELDS FOR CSI/CMSI PROJECT:
      *                  MA-CREATION-STATUS
      *                  MA-CREATION-LI-VENDOR    (88)
      *                  MA-CREATION-LOAN-MANAGER (88)
      *
      *  KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 1999.1018 ADDED MA-CREATION-STATUS FOR CMSI/CSI PROJECT(LI)
      *               88  MA-CREATION-EXTERNAL            VALUE "L".
      *               88  MA-CREATION-INTERNAL            VALUE "I" " ".
      *               88  MA-CREATION-LI-VENDOR           VALUE "L".
      *
      *  KEC 1999.1130 ADDED MA-LI-APPNO FOR CMSI/CSI PROJECT (LI)
      *  KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      *  KEC 2001.1009 {DR#00006} REGIONAL ACC. & MERCURY [KEVIN MULLEN]
      *                HAD TO EXPAND THE VENDOR APPLICATION NUMBER
      *                FROM 6 DIGITS TO 8 DIGITS.  ADDED THE FOLLOWING:
      *
      *         03  MA-VENDOR-APPNO                   PIC 9(08).
      *         03  FILLER                  REDEFINES MA-VENDOR-APPNO.
      *             05  MA-LI-APPNO                   PIC 9(06).
      *             05  MA-LI-APPNO-EXTRA             PIC 9(02).
      *             05  MA-LI-APPNO-EXTRAX  REDEFINES MA-LI-APPNO-EXTRA
      *                                               PIC X(02).
      *
      * KEC 2004.1117 {PR#02407} LENDMARK <A15> [KEVIN MULLEN]
      *               ADDED FIELD:  03  MA-SRCD-ADV  PIC X(02).
      *
      * KEC 2004.1130 {PR#00088} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *               ADDED 78 MA-SIZE   VALUE 225             RSZ=0225
      *
      * KEC 2005.0103 {PR#02439} LENDMARK <A15> [KEVIN MULLEN]
      *               CHANGED 'MA-SRCD-ADV' FROM X(2) TO X(6).
      *
      ******************************************************************
       78  MA-SIZE                                            VALUE 225.
      *=================================================================
       01  MA-REC.
           03  MA1-KEY.
               05  MA-APPNO                      PIC 9(06).

           03  MA2-KEY.
               05  MA-SSNO                       PIC 9(09).
               05  MA-APPDATE                    PIC 9(07)      COMP-3.

           03  MA3-KEY.
               05  MA-LNAME                      PIC X(16).
               05  MA-FNAME                      PIC X(16).

           03  MA4-KEY.
               05  MA-REPCODE                    PIC X(04).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NON-CONTIGUOUS KEY:
      *        MA5-KEY --- DLNO, STATUS DATE, LAST NAME, FIRST NAME.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *=================================================================
      *
      *    MASTER APPLICATION GENERAL INFORMATION
      *
      *=================================================================
           03  MA-ACCTNO                         PIC 9(08)       COMP-3.

           03  MA-ADDRESS-INFO.
               05 MA-ADR1                        PIC X(24).
               05 MA-ADR2                        PIC X(24).
               05 MA-CITY                        PIC X(15).
               05 MA-STATE                       PIC X(02).
               05 MA-ZIP                         PIC X(10).

           03  MA-APP-AMOUNT                     PIC S9(7)V99    COMP-3.
           03  MA-BRANCH                         PIC 9(04)       COMP-3.
           03  MA-CLASS                          PIC 9(02).
           03  MA-CREDIT-SCORE                   PIC 9(04)       COMP-3.
           03  MA-DLNO                           PIC 9(04)V99    COMP-3.
           03  MA-PURCD                          PIC X(03).
           03  MA-REFUSAL-LETTER-FG              PIC X(01).
           03  MA-SRCD                           PIC X(02).
           03  MA-STATUS-CD                      PIC X(03)     OCCURS 4.

      *=================================================================
      *
      *    MASTER APPLICATION STATUS
      *
      *-----------------------------------------------------------------
      *    MA-STATUS
      *        P    - PENDING (WAITING TO BE APPROVED FOR DEALER)
      *        C    - CONDITIONAL APPROVAL
      *        A    - APPROVED APPLICATION FOR DEALER
      *        M    - MADE LOAN
      *        T    - TURNED DOWN (REFUSAL)
      *        U    - TURNED US DOWN (REFUSED TO TAKE LOAN)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   ?  M,T,U  - CAN HAVE DUPLICATE RECORDS
      *   ?  P,A,C  - CAN NOT HAVE DUPLICATE RECORDS
      *=================================================================
           03  MA-STATUS                         PIC X(01).
               88  MA-STATUS-PENDING                          VALUE "P".
               88  MA-STATUS-CONDITIONAL                      VALUE "C".
               88  MA-STATUS-APPROVED                         VALUE "A".
               88  MA-STATUS-MADE                             VALUE "M".
               88  MA-STATUS-TURNDOWN                         VALUE "T".
               88  MA-STATUS-TURNUSDOWN                       VALUE "U".

           03  MA-STATUS-DATE                    PIC 9(07)       COMP-3.
           03  MA-STATUS-TIME                    PIC 9(06)       COMP-3.
           03  MA-STATUS-USERID                  PIC X(10).

      *=================================================================
      *
      *    MASTER APPLICATION TRACKING STATUS
      *
      *-----------------------------------------------------------------
      *    MA-TRACK-STATUS --- USED TO TRACK ALL STATUSES USED.
      *
      *    'CURRENT'   = CURRENT STATUS
      *    'PREVIOUS'  = PREVIOUS STATUS [ BACKWARD AND FORWARD ]
      *    'PREV-BACK' = PREVIOUS STATUS - BACKWARD [ NOT IN USE ]
      *    'PREV-FWD'  = PREVIOUS STATUS - FORWARD  [ IN USE ]
      *    'PREV-USED' = PREVIOUS USED - [ PREVIOUS(BACKWARD)/OTHER ]
      *    'UNUSED'    = NOT IN USE [ SPACE/PREVIOUS(BACKWARD) ]
      *    'USED'      = IN USE [ CURRENT/PREVIOUS(FORWARD)/OTHER ]
      *
      *-----------------------------------------------------------------
      *      ACTION           [ EXAMPLE ]    P   C   A   M   T   U
      *      -------------------------------------------------------
      *      NEW RECORD (PENDING)          | C |   |   |   |   |   |
      *      PENDING     TO CONDITIONAL    | F | C |   |   |   |   |
      *      CONDITIONAL TO APPROVED       | U | F | C |   |   |   |
      *      APPROVED    TO TURNUSDOWN     | U | U | F |   |   | C |
      *       ...                          |   |   |   |   |   |   |
      *      TURNUSDOWN  TO PENDING        | C | X | X |   |   | B |
      *      PENDING     TO CONDITIONAL    | F | C |   |   |   |   |
      *       ...                          |   |   |   |   |   |   |
      *      TURNUSDOWN  TO APPROVED       | U | F | C |   |   | B |
      *      APPROVED    TO CONDITIONAL    | F | C | B |   |   |   |
      *=================================================================
           03  MA-TRACK-STATUS.
               05  MA-TRACK-STATUS-PENDING       PIC X(1) VALUE SPACES.
                   88  MA-TRACK-PENDING-CURRENT           VALUE "C".
                   88  MA-TRACK-PENDING-PREVIOUS          VALUE "B" "F".
                   88  MA-TRACK-PENDING-PREV-BACK         VALUE "B".
                   88  MA-TRACK-PENDING-PREV-FWD          VALUE "F".
                   88  MA-TRACK-PENDING-PREV-USED         VALUE "B" "X".
                   88  MA-TRACK-PENDING-UNUSED            VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-PENDING-USED              VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MA-TRACK-STATUS-CONDITIONAL   PIC X(1) VALUE SPACES.
                   88  MA-TRACK-CONDITIONAL-CURRENT       VALUE "C".
                   88  MA-TRACK-CONDITIONAL-PREVIOUS      VALUE "B" "F".
                   88  MA-TRACK-CONDITIONAL-PREV-BACK     VALUE "B".
                   88  MA-TRACK-CONDITIONAL-PREV-FWD      VALUE "F".
                   88  MA-TRACK-CONDITIONAL-PREV-USED     VALUE "B" "X".
                   88  MA-TRACK-CONDITIONAL-UNUSED        VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-CONDITIONAL-USED          VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MA-TRACK-STATUS-APPROVED      PIC X(1) VALUE SPACES.
                   88  MA-TRACK-APPROVED-CURRENT          VALUE "C".
                   88  MA-TRACK-APPROVED-PREVIOUS         VALUE "B" "F".
                   88  MA-TRACK-APPROVED-PREV-BACK        VALUE "B".
                   88  MA-TRACK-APPROVED-PREV-FWD         VALUE "F".
                   88  MA-TRACK-APPROVED-PREV-USED        VALUE "B" "X".
                   88  MA-TRACK-APPROVED-UNUSED           VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-APPROVED-USED             VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MA-TRACK-STATUS-MADE          PIC X(1) VALUE SPACES.
                   88  MA-TRACK-MADE-CURRENT              VALUE "C".
                   88  MA-TRACK-MADE-PREVIOUS             VALUE "B" "F".
                   88  MA-TRACK-MADE-PREV-BACK            VALUE "B".
                   88  MA-TRACK-MADE-PREV-FWD             VALUE "F".
                   88  MA-TRACK-MADE-PREV-USED            VALUE "B" "X".
                   88  MA-TRACK-MADE-UNUSED               VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-MADE-USED                 VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MA-TRACK-STATUS-TURNDOWN      PIC X(1) VALUE SPACES.
                   88  MA-TRACK-TURNDOWN-CURRENT          VALUE "C".
                   88  MA-TRACK-TURNDOWN-PREVIOUS         VALUE "B" "F".
                   88  MA-TRACK-TURNDOWN-PREV-BACK        VALUE "B".
                   88  MA-TRACK-TURNDOWN-PREV-FWD         VALUE "F".
                   88  MA-TRACK-TURNDOWN-PREV-USED        VALUE "B" "X".
                   88  MA-TRACK-TURNDOWN-UNUSED           VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-TURNDOWN-USED             VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  MA-TRACK-STATUS-TURNUSDOWN    PIC X(1) VALUE SPACES.
                   88  MA-TRACK-TURNUSDOWN-CURRENT        VALUE "C".
                   88  MA-TRACK-TURNUSDOWN-PREVIOUS       VALUE "B" "F".
                   88  MA-TRACK-TURNUSDOWN-PREV-BACK      VALUE "B".
                   88  MA-TRACK-TURNUSDOWN-PREV-FWD       VALUE "F".
                   88  MA-TRACK-TURNUSDOWN-PREV-USED      VALUE "B" "X".
                   88  MA-TRACK-TURNUSDOWN-UNUSED         VALUE " " "B"
                                                                "X".
                   88  MA-TRACK-TURNUSDOWN-USED           VALUE "U" "C"
                                                                "F".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  MA-TRACK-STATUS-TABLE   REDEFINES MA-TRACK-STATUS.
               05  MA-TRACK-STATUSX              OCCURS 6 TIMES.
                   07  MA-TRACK-STATUS-ELE       PIC X(01).

      *=================================================================
      *    MA-CREATION-STATUS:  USED TO KNOW IF CREATED BY A OUTSIDE
      *                         VENDOR (EXTERNAL) OR BY THE LOAN MANAGER
      *                         (INTERNAL).
      *-----------------------------------------------------------------
           03  MA-CREATION-STATUS                PIC X(1).
               88  MA-CREATION-EXTERNAL              VALUE "L".
               88  MA-CREATION-INTERNAL              VALUE "I" " ".
               88  MA-CREATION-LI-VENDOR             VALUE "L".

      *-----------------------------------------------------------------
           03  MA-VENDOR-APPNO                   PIC 9(08).
           03  FILLER                  REDEFINES MA-VENDOR-APPNO.
               05  MA-LI-APPNO                   PIC 9(06).
               05  MA-LI-APPNO-EXTRA             PIC 9(02).
               05  MA-LI-APPNO-EXTRAX  REDEFINES MA-LI-APPNO-EXTRA
                                                 PIC X(02).

      *-----------------------------------------------------------------
           03  MA-SRCD-ADV                       PIC X(06).
           03  FILLER                            PIC X(15).
