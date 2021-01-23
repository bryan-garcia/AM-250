Assignment: Game of Life final project
Programmer: Bryan Garcia
Course: AM 250

Description: 
    Game of Life implemented with MPI. *Column-wise* domain decomposition version. 
    
    ...I'll get the "Peta" version running one day :)

Contents:
    
    mpi_gol : Directory containing all source code, error log folder, output log folder, and job folder.

        - gol_mpi.f90: Driver code for game of life.
        - gol_helper.f90: Module containing subroutines for the various game of life required tasks.
        - gol_time.f90: Code I used to obtain timing results seen in report.

How to run me:

    Batch:

        While on grape and in the "mpi_gol" directory, simply type "make batch"
        The .cmd file is set to run on 4 processors. Said .cmd file be found in "mpi_gol/jobs"
        Edit if you'd like. 

        All batch output will be found in the "mpi_gol/output" directory. View the output with
        whatever you'd like (more, vi, cat?, ...)
        
        Make sure to "make clean" after each batch submission so you don't mix up batch output logs!


    Interactively:

        While on a grape compute node, make your way to the "mpi_gol" directory and type
        "make gol". From here, use "mpirun" to execute "gol_mpi" with your desired number of
        processors
    

Other notes:

    The source code is set to run on a 20 x 20 grid. Edits can be made by changing the value of "col_height" 
    in "gol_mpi.f90"
