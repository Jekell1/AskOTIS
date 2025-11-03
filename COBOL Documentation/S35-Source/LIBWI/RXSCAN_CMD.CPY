      * COPYMEMBER: LIBWI/RXSCAN.CMD
      *-----------------------------------------------------------------
      *    R X S C A N - B U F
      *-----------------------------------------------------------------
       01  RXSCAN-LEGENDS.
           03  BEG-BRNO-MSEL.
               05  FILLER            PIC X(37)         VALUE
               "F1-Reset, Enter Beginning Branch # or".
               05  FILLER            PIC X(23)         VALUE
               " Group, F3-ALL,F7-Exit:".

           03  END-BRNO-MSEL         PIC X(46)         VALUE
               "F1-Reset, Enter Ending Branch Number, F7-Exit:".

           03  BEG-DLNO-MSEL.
               05  FILLER            PIC X(48)         VALUE
               "F1-Reset, Enter Beginning Dealer Number, F3-ALL,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

           03  END-DLNO-MSEL.
               05  FILLER            PIC X(37)         VALUE
               "F1-Reset, Enter Ending Dealer Number,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

           03  BEG-INTDATE-MSEL.
               05  FILLER            PIC X(48)         VALUE
               "F1-Reset, Enter Beginning Interest Date, F3-ALL,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

           03  END-INTDATE-MSEL.
               05  FILLER            PIC X(37)         VALUE
               "F1-Reset, Enter Ending Interest Date,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

           03  BEG-LNAME-MSEL.
               05  FILLER            PIC X(27)         VALUE
               "F1-Reset, Enter Last Name, ".
               05  FILLER            PIC X(29)         VALUE
               "F5-Toggle/Account #, F7-Exit:".

           03  END-LNAME-MSEL.
               05  FILLER            PIC X(33)         VALUE
               "F1-Reset, Enter Ending Last Name,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

           03  BEG-ACCTNO-MSEL.
               05  FILLER            PIC X(27)         VALUE
               "F1-Reset, Enter Account #, ".
               05  FILLER            PIC X(31)         VALUE
               "F5-Toggle/Last Name, F7-Exit:  ".

           03  END-ACCTNO-MSEL.
               05  FILLER            PIC X(38)         VALUE
               "F1-Reset, Enter Ending Account Number,".
               05  FILLER            PIC X(9)         VALUE
               " F7-Exit:".

           03  BEG-LOAN-AMOUNT-MSEL.
               05  FILLER            PIC X(30)         VALUE
               "F1-Reset, Enter Beginning Loan".
               05  FILLER            PIC X(22)         VALUE
               " Amount, F3-ALL, F7-X:".

           03  END-LOAN-AMOUNT-MSEL.
               05  FILLER            PIC X(35)         VALUE
               "F1-Reset, Enter Ending Loan Amount,".
               05  FILLER            PIC X(9)          VALUE
               " F7-Exit:".

      *kc->03  BEG-STATUS-MSEL.
      *        05  FILLER            PIC X(47)         VALUE
      *        "F1-Reset, Enter Rescission Status Code, F3-ALL,".
      *        05  FILLER            PIC X(9)          VALUE
      *kc->    " F7-Exit:".

           03  BEG-STATUS-MSEL.
               05  FILLER PIC X(25) VALUE "F1-Reset, F7-Exit, Enter ".
               05  FILLER PIC X(25) VALUE "(I)nrescission or (R)esci".
               05  FILLER PIC X(06) VALUE "ded  :".

           03  BEG-SELECTION-MSEL.
               05  FILLER            PIC X(40)         VALUE
               "F1-Reset, F7-Exit, Enter on Loan Number:".
