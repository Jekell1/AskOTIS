      * COPYMEMBER: LIBLP/LPFRFD
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFRFD
      *
      * DESCRIPTION   :  FORM DESCRIPTOR FILE 'ROUTINES'
      *
      *=================================================================
      * REV:
      * KEC 2005.0809 ***NEW***
      * KEC 2007.1031 ADDED 'LPWSFD-FORM-NAM' TO 'LPFRFD-FIND-PREFIX'.
      * KEC 2011.1003 ADDED 'PST' (POSTSCRIPT) OPTION.
      *
      ******************************************************************
      *
      *    L P F R F D - F I N D - P R E F I X
      *
      *=================================================================
      *    IN  : FD-REC
      *
      *    OUT : LPWSFD-PREFIX   {LONP/PCL/PST/PRT}
      *          LPWSFD-FORM-NAM
      *
      *    DESC: FIND IF THE PROGRAM IS 'LONP', 'PCL', 'PST' OR 'PRT'.
      *
      ******************************************************************
       LPFRFD-FIND-PREFIX SECTION.

           IF ( FD-OPTION-PCL-LASER )
              MOVE LPWSFD---PREFIX-PCL     TO LPWSFD-PREFIX
           ELSE
           IF ( FD-OPTION-PST-LASER )
              MOVE LPWSFD---PREFIX-PST     TO LPWSFD-PREFIX
           ELSE
           IF ( FD-PROG-PREFIX-PRT )
              MOVE LPWSFD---PREFIX-PRT     TO LPWSFD-PREFIX
           ELSE
              MOVE LPWSFD---PREFIX-LONP    TO LPWSFD-PREFIX.

           PERFORM LPFRFD-LOAD-FORM-NAM.

       LPFRFD-FIND-PREFIX-EXIT.
           EXIT.

      ******************************************************************
      *
      *    L P F R F D - L O A D - F O R M - N A M
      *
      *=================================================================
      *    IN  : FD-REC
      *          LPWSFD-PREFIX   {LONP/PCL/PST/PRT}
      *
      *    OUT : LPWSFD-FORM-NAM
      *
      *    DESC: LOAD THE PROGRAM NAME.
      *
      ******************************************************************
       LPFRFD-LOAD-FORM-NAM SECTION.

           IF ( LPWSFD-PREFIX-PCL )
              MOVE LPWSFD-FORM-NAM---PCL   TO LPWSFD-FORM-NAM
              MOVE FD-PROG-PREFIX          TO LPWSFD-FORM-PROG-PREFIX
              MOVE FD-PROG                 TO LPWSFD-FORM-PROG
           ELSE
           IF ( LPWSFD-PREFIX-PST )
              MOVE LPWSFD-FORM-NAM---PST   TO LPWSFD-FORM-NAM
              MOVE FD-PROG-PREFIX          TO LPWSFD-FORM-PROG-PREFIX
              MOVE FD-PROG                 TO LPWSFD-FORM-PROG
           ELSE
           IF ( LPWSFD-PREFIX-PRT )
              MOVE LPWSFD-FORM-NAM---PRT   TO LPWSFD-FORM-NAM
              MOVE FD-PROG-PREFIX          TO LPWSFD-FORM-PROG-PREFIX
              MOVE FD-PROG                 TO LPWSFD-FORM-PROG
           ELSE
              MOVE LPWSFD-FORM-NAM---LONP  TO LPWSFD-FORM-NAM
              MOVE FD-PROG                 TO LPWSFD-FORM-PROG.

       LPFRFD-LOAD-FORM-NAM-EXIT.
           EXIT.
