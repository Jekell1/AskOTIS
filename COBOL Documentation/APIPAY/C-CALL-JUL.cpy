      *********************************************************************
      *  NAME: C-CALL-JUL
      *  DESC: CONVERT GREGORIAN CALANDER DATE TO A JULIAN DATE
      *  IN  : DATE IN CCYYMMDD FORMAT
      *  OUT : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *********************************************************************
       C-CALL-JUL SECTION.
           MOVE P-DATE-IN TO JJUL-IN.
           COMPUTE JWS-JUL-IN-CCYY = JJUL-IN-CCYY - 1900.

           MOVE JJUL-IN-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.

           IF JJUL-IN-MM > 2 AND LEAP-YEAR-TRUE
              MOVE 1 TO JWS-EXTRA-DAY
           ELSE
              MOVE 0 TO JWS-EXTRA-DAY.

           COMPUTE P-JULIAN-DATE = ( (JWS-JUL-IN-CCYY - 1) * 1461 / 4 )
                                + DYTOM(JJUL-IN-MM) + JJUL-IN-DD +
                                JWS-EXTRA-DAY.

      *********************************************************************
      *  NAME: C-CALL-CJUL
      *  DESC: CONVERT JULIAN DATE TO A GREGORIAN CALANDER DATE
      *  IN  : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *  OUT : DATE IN CCYYMMDD FORMAT
      *********************************************************************
