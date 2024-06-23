This is the github repository for the paper \`\`A Burden Shared is a Burden Havled: A Fairness Adjusted Approach to Classification" by Bradley Rava, Wenguang Sun, Gareth James, and Xin Tong.

This repository contains the code required to reproduce the figures, simulations, and real data analysis in the main paper. To reproduce any of the analyses, navigate to the *code* folder and pick either the *real data* or *simulation* folder.

Each one of these folders has a dedicated sub-folder for each analysis in the main paper. These folders are self contained. They all contain the following files.

1.  **Main File**. This file should be opened by the user and it will run the main analysis.
2.  **Function File**. This file does not need to be opened, but it will be called by the main file. The function file has all needed functions to run the analysis in the main file.
3.  **Plot File**. After finishing the analysis in the main file, you can produce the corresponding plot in the paper by running the code in the plot file.

When running any of the main files, make sure that your current working directory is set to the same folder it is contained in. That way the function file and data can be sourced properly.

Please feel free to reach out if you have any questions - [bradley.rava\@sydney.edu.au](mailto:bradley.rava@sydney.edu.au){.email}.
