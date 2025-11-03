      * COPYMEMBER: LIBCL/CRNOR2W
      **************************************************************************
      *          LIBCL/CRNOR2W
      *
      * USED TO DYNAMICALLY BUILD A NOTICE RECORD:
      *
      * REV:
      *  JTG 032392 ADDED CDEL-ARREARAGE, AVCR-RESTRICT-FG FOR COLLECTION SYSTEM
      *  JTG 060892 ADDED BR-PDTH-NXDUE
      *  JTG 061695 ADDED BR-NOT-OURS-REFUND
      *  BAH 011696 ADDED CRNOR2-LPTRCD TO GET A TRUE "PO" PAYOFF
      *  JTG 102896 ADDED CRNOR2-GET-PAYOFF-ONLY, FOR PAYOFF GOOD THRU ROUTINE
      *  JTG 122297 ADDED CRNOR2-RECAST-FG RE: IDEAL A48 TO A60
      *  BLM 170309 CHANGE CRNOR2-SYSDATE TO 9(8), PR# PD0006
      *  BAH 180717 ADDED CRNOR2-INDU-PENALTY SO COLAST COULD CALL CRNOR2
      *               AND CRNOR2-CPOT30, RDPOT30, CPOTCHGOFF, RDPOTCHGOFF
      *  BAH 2024.0201 ADDED THE ABOVE MENTIONED FIELDS (180717) TO THE NOTICE
      *                RECORD AND REMOVED THEM FROM THE OUTPUT HERE SO YOU CAN
      *                READ THE NOTICE AND HAVE THEM AND NOT HAVE TO CALL THIS 
      **************************************************************************
       01  CRNOR2-LINK.
      * INPUT
           03  CRNOR2-ACCTNO                PIC 9(8).
           03  CRNOR2-SYSDATE               PIC 9(8).
           03  CRNOR2-LPTRCD                PIC XX     VALUE "PY".
           03  CRNOR2-GET-PAYOFF-ONLY       PIC X      VALUE " ".
           03  CRNOR2-RECAST-FG             PIC X      VALUE " ".
           03  CRNOR2-DEF-STOP              PIC X      VALUE " ".
           03  CRNOR2-LPREC-FG              PIC X      VALUE " ".
           03  FILLER                       PIC X(24)  VALUE SPACES.

      * OUTPUT
           03  FILLER                       PIC X(25)  VALUE SPACES.
