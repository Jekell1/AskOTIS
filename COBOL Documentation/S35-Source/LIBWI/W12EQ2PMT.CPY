      * COPYMEMBER: LIBWI/W12EQ2PMT
      **************************************************************************
      *                   LIBWI/W12EQ2PMT
      *
      * DESC:   DETERMINE THE PAYMENT AMOUNTS AFTER SPREADING THE
      *         EXTENSION CHARGE VIA: FORMULA 22
      *
      * IN  :   W12EQ2-ORGTERM
      *         W12EQ2-EXTCHG
      *         W12EQ2-1STPYPMT
      *         W12EQ2-REGPYAMT
      *         W12EQ2-LASTPYAMT
      *
      * OUT :   W12EQ2-1STPYPMT
      *         W12EQ2-REGPYAMT
      *         W12EQ2-LASTPYAMT
      * REV:
      *  JTG 030430 *NEW* CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *             OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                           CITIZENS #2006
      *  JTG 030610 CHANGED TO FORCE PD-REGPYAMT TO PD-1STPYAMT WHEN
      *             NO ODDPYAMT                                   CITIZENS #2006
      *  JTG 030617 CHANGED TO HANDLE BALLOONS BY ADDED SPREAD TO LAST
      *             LASTPAYAMT                                    CITIZENS #2006
      **************************************************************************
       GET-W12EQ2-NEW-PAYMENTS SECTION.
    
           IF NOT SP-PYEXFRMLA-SAME2-SPREAD
              GO TO GET-W12EQ2-NEW-PAYMENTS-EXIT.
    
           IF W12EQ2-1STPYAMT = 0
              MOVE W12EQ2-REGPYAMT TO W12EQ2-1STPYAMT.
           DIVIDE W12EQ2-ORGTERM INTO W12EQ2-EXTCHG
                                GIVING W12EQ2-SPREAD
                                REMAINDER W12EQ2-REM.
           SUBTRACT W12EQ2-EXTCHG FROM W12EQ2-1STPYAMT.
           ADD W12EQ2-SPREAD        TO W12EQ2-1STPYAMT.
    
           IF W12EQ2-ORGTERM = 1
              ADD W12EQ2-SPREAD            TO W12EQ2-REGPYAMT
           ELSE
           IF (W12EQ2-REGPYAMT = W12EQ2-LASTPYAMT) OR
              (W12EQ2-LASTPYAMT = 0              )
              ADD W12EQ2-SPREAD            TO W12EQ2-REGPYAMT
              ADD W12EQ2-REGPYAMT W12EQ2-REM GIVING W12EQ2-LASTPYAMT
           ELSE
              ADD W12EQ2-SPREAD            TO W12EQ2-REGPYAMT
              ADD W12EQ2-SPREAD W12EQ2-REM TO W12EQ2-LASTPYAMT.
    
       GET-W12EQ2-NEW-PAYMENTS-EXIT.
           EXIT.
