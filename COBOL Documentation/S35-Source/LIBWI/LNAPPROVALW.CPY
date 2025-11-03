      * COPYMEMBER: LIBWI/LNAPPROVALW
      ******************************************************************
      *
      *  MEMBER NAME: LIBWI/LNAPPROVALW
      *
      *  DESCRIPTION: LOAN APPROVAL WORKERS [STANDARD/WINDOW]
      *
      *-----------------------------------------------------------------
      * REV:
      * BLV 030228 REMOVED SEVERAL MESSAGES, PUT IN LNAPPROVAL
      * BLV 030306 ADDED HOLD-GR-APPROVAL-LIMIT, WORLD PR# 315
      * KEC 050524 PR#2516 PREMIER ADVISORY 
      *            ADDED 'LNAPPROVAL-CREATION'.
      * KEC 061005 PR#2783 PREMIER ADVISORY 
      *            ADDED "LNAPPROVAL-AMT-WORKERS" TO BETTER MANAGE
      *            THE "VERIFY-LOAN-APPROVAL" SECTION.
      * BAH 2024.1106 CHANGED WS-PROCEEDS-AMT AND DAW-PROCEEDS-AMT
      *               TO BE TOTAL-REPAY #1684 S35Q-143
      *
      ******************************************************************
      *
      *    L O A N - A P P R O V A L - W O R K E R S
      *
      *=================================================================
       01  HOLD-APPROVAL-LIMIT        PIC 9(07)   COMP   VALUE ZEROES.
       01  HOLD-GR-APPROVAL-LIMIT     PIC 9(07)   COMP   VALUE ZEROES.
       01  HOLD-APPROVAL-USERID       PIC X(03)          VALUE SPACES.

      *-----------------------------------------------------------------
       01  LNAPPROVAL-AMT-WORKERS.
           03  LNAPPROVAL-AMT-APP     PIC 9(7)V99        VALUE ZEROES.
           03  LNAPPROVAL-AMT-GI      PIC 9(7)V99        VALUE ZEROES.
           03  LNAPPROVAL-AMT-GR      PIC 9(7)V99        VALUE ZEROES.
           03  LNAPPROVAL-AMT-PD      PIC 9(7)V99        VALUE ZEROES.
           03  LNAPPROVAL-AMT-VERIFY  PIC 9(7)V99        VALUE ZEROES.

      *-----------------------------------------------------------------
       01  LNAPPROVAL-APPROVAL-ID     PIC X(04)          VALUE SPACES.

       01  LNAPPROVAL-LEGEND          PIC X(57)          VALUE SPACES.
       01  LNAPPROVAL-LEGEND-LIST     PIC X(05)          VALUE "WSEL.".

       01  LNAPPROVAL-STATUS          PIC X(01)          VALUE SPACES.
           88  LNAPPROVAL-STATUS-APPROVED                VALUE "A".
           88  LNAPPROVAL-STATUS-UNAPPROVED              VALUE "U".

      *=================================================================
      *    DESC: USED TO DETERMINE IF LOAN (PD) WAS CREATED FROM A
      *          LOAN APPLICATION {APP} OR LOAN INPUT {LONP}.
      *-----------------------------------------------------------------
       78  LNAPPROVAL---CREATION-APP                     VALUE "A".
       78  LNAPPROVAL---CREATION-LONP                    VALUE SPACES.
      *-----------------------------------------------------------------
       01  LNAPPROVAL-CREATION        PIC X(01)          VALUE SPACES.
           88  LNAPPROVAL-CREATION-APP                   VALUE "A".
           88  LNAPPROVAL-CREATION-LONP                  VALUE SPACES.

      *=================================================================
      *    WS-APPROVAL-FG IS ALSO USED IN FLAG OF PROMPTS AND ALLOWING
      *     ACCESS TO THE APPROVAL WINDOW.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WS-APPROVAL-FG             PIC X(01)          VALUE SPACES.
           88  WS-APPROVAL-FG-EXIT                       VALUE "X".
           88  WS-APPROVAL-FG-SAVE                       VALUE "S".

       01  WS-TOTAL-REPAY             PIC 9(07)          VALUE ZEROES.
       01  WS-SET-APPROVAL-FG         PIC X(01)          VALUE SPACES.
           88  WS-SET-APPROVAL-APPROVED                  VALUE "A".
           88  WS-SET-APPROVAL-SET-LIMIT                 VALUE "S".

      *=================================================================
      *
      *    L E G E N D - L O A N - A P P R O V A L           (WINDOW #1)
      *
      *=================================================================
       01  LEGEND-01-APPROVAL                         PIC X(46)   VALUE
           "F7-EXIT, (A)PPROVE LOAN, (S)ET APPROVAL LIMIT:".

       01  LEGEND-02-APPROVAL                         PIC X(46)   VALUE
           "F1-RESET, ENTER APPROVAL LIMIT               :".

       01  LEGEND-03-APPROVAL                         PIC X(46)   VALUE
           "F1-RESET, F7-SAVE APPROVAL LIMIT             :".

      *=================================================================
      *
      *    L E G E N D - L O A N - A P P R O V A L           (WINDOW #2)
      *
      *=================================================================
       01  LEGEND-01-APPROVAL2                        PIC X(57)   VALUE
           "F1-RESET,F7-EXIT,(A)PPROVE LOAN,(S)ET LIMIT:".

       01  LEGEND-02-APPROVAL2                        PIC X(57)   VALUE
           "F1-RESET, ENTER APPROVAL LIMIT                          :".

      *=================================================================
      *
      *    M E S S A G E - L O A N - A P P R O V A L
      *
      *=================================================================
       01  MESSAGE-NA-LIMIT                           PIC Z,ZZZ,ZZ9.

       01  MESSAGE-NEED-PASSWORD                      PIC X(54)   VALUE
           "CHANGING THIS LIMIT IS NOT ALLOWED WITHOUT PASSWORD!!!".
      *------------------------------------------------ 'MSG' - MESSAGES
       01  MSG-SCORE-APPROVAL-UPDATE                  PIC X(47)   VALUE
           "APPROVED CREDIT SCORE SET IS LESS THAN CUT-OFF!".

       01  MSG-SCORE-NEED-APPROVAL-01                 PIC X(39)   VALUE
           "BORROWER CREDIT SCORE IS BELOW CUT-OFF!".

       01  MSG-SCORE-NEED-APPROVAL-02.
           03  FILLER                                 PIC X(18)   VALUE
               "BORROWER SCORE IS ".
           03  MSG-BW-SCORE                           PIC ZZZ9.
           03  FILLER                                 PIC X(22)   VALUE
               ";   APPROVED SCORE IS ".
           03  MSG-PD-SCORE                           PIC ZZZ9.

       01  MSG-SCORE-NEED-APPROVAL-03                 PIC X(47)   VALUE
           "PLEASE SAVE THIS LOAN AND CONTACT YOUR DIRECTOR".

       01  MSG-SCORE-NEED-APPROVAL-04                 PIC X(55)   VALUE
           "BORROWER CREDIT SCORE BELOW CUT-OFF; APPROVAL REQUIRED!".

       01  MSG-SCORE-NEED-APPROVAL-05.
           03  FILLER                                 PIC X(18)   VALUE
               "BORROWER SCORE IS ".
           03  MSG-BW-SCORE2                          PIC ZZZ9.
           03  FILLER                                 PIC X(21)   VALUE
               ";   CUT-OFF SCORE IS ".
           03  MSG-SP-SCORE                           PIC ZZZ9.

       01  MSG-SCORE-NEED-PASSWORD                    PIC X(55)   VALUE
           "CHANGING THIS CUT-OFF IS NOT ALLOWED WITHOUT PASSWORD!!".

      *=================================================================
      *
      *    D I S P L A Y - A P P R V - W I N D O W (DAW)
      *
      *=================================================================
       01  DISPLAY-APPROVAL-WINDOW-BUF.
           03  DISPLAY-LIMIT-WINDOW-BUF.
               05  DAW-TOTAL-REPAY                    PIC Z,ZZZ,ZZ9.
               05  DAW-GLIMIT                         PIC Z,ZZZ,ZZ9.
               05  DAW-010                            PIC Z,ZZZ,ZZ9.
               05  DAW-APPROVAL-ID                    PIC X(03).

      *-----------------------------------------------------------------
       01  DISPLAY-APPROVAL-WINDOW-LIST.

           03  DISPLAY-LIMIT-WINDOW-LIST.
               05  FILLER                             PIC X(35)   VALUE
                   "FINANCE-AMT GLIMIT 010 APPROVAL-ID.".
