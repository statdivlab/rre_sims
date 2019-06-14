# Efficacy Simulations

This code reproduces the simulations, figures and tables in Section 4 of the paper.  The basic pipeline to reproduce is

1.  Run main_creation.R on a personal computer, transfer the directory to a cluster system.
2.  Run all shell scripts beginning with qsub (e.g. qsub_batch_01.sh) on the cluster and transfer the directory back.
3.  Run main_aggregation.R and figures_tables.R on a personal computer.
