      * COPYMEMBER: LIBLP/LP01VSP
      ******************************************************************
      *
      * DESCRIPTION: VISION VERSION OF THE STATE PARAMETER FILE (SPFILE)
      *
      * VSP1-KEY   : VSP-ORGST, VSP-SPRCLASS, VSP-SUBCLASS, VSP-LAWCODE
      *
      *=================================================================
      * VSPFILE NEEDED IN PROGRAMS THAT EXECUTED IN A MASTER & SECONDARY
      * MANNER.  FOR EXAMPLE, ACRERN DOES BRANCH-GROUP WHICH CALLS TO
      * AC1ERN FOR EACH BRANCH.  SO IN ACRERN, IT WILL DO
      * CREATE-VSP-FILE THEN PASS VSP-PATH TO AC1ERN FOR EACH BRANCH
      * SO WE DO NOT NEED TO RECREATE IT (LIKE ARRAYSP WOULD DO).
      *=================================================================
      * REV:
      * JKC 2023-1218 ***NEW***
      *
      ******************************************************************
       78  VSP-SIZE                                  VALUE 6509.
       78  VSP-SP-SIZE                               VALUE 6500.
       01  VSP-REC.
           03  VSP1-KEY.
               05  VSP-ORGST                         PIC X(2).
               05  VSP-SPRCLASS                      PIC 9(3).
               05  VSP-SUBCLASS                      PIC 9(2).
               05  VSP-LAWCODE                       PIC 9(2).
           03  VSP-SP-REC                            PIC X(VSP-SP-SIZE).

