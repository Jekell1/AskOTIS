      * COPYMEMBER: LIBLP/LPFRVSP
      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPFRVSP
      *
      * DESCRIPTION   : TEMPORARY VISION FILE CREATION - VSPFILE
      *
      *=================================================================
      * VSPFILE NEEDED IN PROGRAMS THAT EXECUTED IN A MASTER & SECONDARY
      * MANNER.  FOR EXAMPLE, ACRERN DOES BRANCH-GROUP WHICH CALLS TO
      * AC1ERN FOR EACH BRANCH.  SO IN AC1ERN, FOR THE FIRST TIME,
      * IT WILL DO CREATE-VSP-FILE THEN PASS VSP-PATH TO AC1ERN FOR
      * EACH BRANCH SO WE DO NOT NEED TO RECREATE IT
      * (LIKE ARRAYSP WOULD DO).
      *
      *=================================================================
      * REV :
      * JKC 2023-1218 ***NEW***
      *
      ******************************************************************
      *
      *   C R E A T E - V S P - F I L E
      *
      *=================================================================
      * IN  : SP-PATH-OVERRIDE
      *       FILPATH-BASE
      *       FILPATH-MACHINE
      *       FILPATH-DATA
      * OUT : VSP-FILE                                    
      * DESC: THIS LOGIC LOOPS THRU THE SP-FILE AND WRITES TO THE
      *       VSP-FILE.
      ******************************************************************
       CREATE-VSP-FILE SECTION.

           IF ( VSP-PATH NOT = SPACES )
              GO TO CREATE-VSP-FILE-EXIT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VSP-PATH..

           PERFORM OPEN-VSP1-FILE-OUTPUT.
           PERFORM CLOSE-VSP1-FILE.
           PERFORM OPEN-VSP1-FILE.

           PERFORM OPEN-SP1-FILE.

           MOVE SPACES                 TO SP-ORGST.
           MOVE ZEROES                 TO SP-SPRCLASS
                                          SP-SUBCLASS
                                          SP-LAWCODE.
 
           PERFORM START-SP1-FILE.

      *=================================================================
       CREATE-VSP-NEXT-SP1.

           PERFORM READ-SP1-FILE-NEXT.

           IF IO-FG NOT = 0
              GO TO CREATE-VSP1-FILE-END.

           MOVE SPACES                 TO VSP-REC.
           MOVE SP1-KEY                TO VSP1-KEY.
           MOVE SP-REC                 TO VSP-SP-REC. 
             
           PERFORM WRITE-VSP1-FILE.

           GO TO CREATE-VSP-NEXT-SP1.

      *=================================================================
       CREATE-VSP1-FILE-END.

           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-VSP1-FILE. 

      *-----------------------------------------------------------------
       CREATE-VSP-FILE-EXIT.
           EXIT.

