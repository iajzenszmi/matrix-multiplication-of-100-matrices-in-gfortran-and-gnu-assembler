      PROGRAM MATRIXMULT
      IMPLICIT NONE
      INTEGER I, J, K, L
      DOUBLE PRECISION MATRICES(3,3,100), RESULT(3,3), TEMP(3,3)

!     Initialize matrices with some values.
      DO L = 1, 100
         DO I = 1, 3
            DO J = 1, 3
               MATRICES(I,J,L) = I * J * L
            ENDDO
         ENDDO
      ENDDO

!     Begin matrix multiplication. Initialize result with the first matrix.
      DO I = 1, 3
         DO J = 1, 3
            RESULT(I,J) = MATRICES(I,J,1)
         ENDDO
      ENDDO

!     Multiply matrices.
      DO L = 1, 100
         PRINT *, 'Matrix number: ', L
         PRINT *, MATRICES(:,:,L)
         DO I = 1, 3
            DO J = 1, 3
               TEMP(I,J) = 0.0D0
               DO K = 1, 3
                  TEMP(I,J) = TEMP(I,J) + RESULT(I,K) * MATRICES(K,J,L)
               ENDDO
            ENDDO
         ENDDO
!        Update result matrix.
         DO I = 1, 3
            DO J = 1, 3
               RESULT(I,J) = TEMP(I,J)
            ENDDO
         ENDDO
      ENDDO

!     Print the result.
      PRINT *, 'Resultant Matrix:'
      DO I = 1, 3
         PRINT *, RESULT(I,:)
      ENDDO

      END PROGRAM
