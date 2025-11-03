      * COPYMEMBER: LIBWI/RXSCANW
      ******************************************************************
      *
      *    R X S C A N - B U F
      *
      * REV:
      * KEC 2017.0602 INCREASED RXSCAN-BEG-ACCTNO TO 9(8).
      * BAH 2024.0425 MADE 01 LEVEL, CALL NOT WINDOW
      *
      ******************************************************************
       01  RXSCAN-BUF.
           03  RXSCAN-BEG-ACCTNO      PIC 9(8).
           03  RXSCAN-RETURN          PIC X.
      ******************************************************************
      * RXSCAN-RETURN-TYPE --- 'A' = ALL TYPES
      *                        'I' = IN RESCISSION TYPES
      *                        'R' = RESCINDED
      ******************************************************************
           03  RXSCAN-RETURN-TYPE         PIC X   VALUE "A".
               88  RXSCAN-RETURN-TYPE-ALL         VALUE "A" "I" "R".
               88  RXSCAN-RETURN-TYPE-I           VALUE "I".
               88  RXSCAN-RETURN-TYPE-R           VALUE "R".
