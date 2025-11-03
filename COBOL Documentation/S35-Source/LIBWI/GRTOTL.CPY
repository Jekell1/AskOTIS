      * COPYMEMBER: LIBWI/GRTOTL
      ******************************************************************
      *
      *    G R T O T L - W I N D O W
      *
      *-----------------------------------------------------------------
      *
      *    THIS COPY MEMBER IS NEEDED WHEN USING "WI/GRTOTL.C".
      *
      *    THIS FOR THE 'ENTER-PARMETERS' SECTION!
      *
      *    NEED:
      *      ENT-GROUP; TOTAL-LEV-TABLE; COPY "LIBGB/GRTOTLW.CPY".
      *
      ******************************************************************
       GRTOTL-WINDOW SECTION.
       CALL-GRTOTL.

           MOVE ENT-GROUP            TO GRTOTL-ENT-GROUP.
           MOVE GTT-SV-ENT-GROUP     TO GRTOTL-SV-ENT-GROUP.
           MOVE TOTAL-LEV-TABLE      TO GRTOTL-TOT-LEV-TABLE.
           MOVE "WI/GRTOTL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRTOTL-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           MOVE GRTOTL-TOT-LEV-TABLE TO TOTAL-LEV-TABLE.
           MOVE ENT-GROUP            TO GTT-SV-ENT-GROUP.

       EXIT-GRTOTL-WINDOW.
           EXIT.
