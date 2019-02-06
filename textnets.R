library(textnets)                                       # Holds all of the textnet function calls from Dr. Bail
data("sotu")                                            # A data set of all of the State of the Union Speeches

sotu_first_speeches <- sotu %>%                         # Create a new dataset
  group_by(president) %>%                               # Group by the variable "president"
  slice(1L)                                             # only take the first speech

prepped_sotu <- PrepText(sotu_first_speeches,           # call preptext on the above dataset
                         groupvar = "president",        # group by the presidents 
                         textvar = "sotu_text",         # the text variable is the text of the speech
                         node_type = "groups",          # setting up nodes by groups
                         tokenizer = "words",           # tokenize all of the words of the speech
                         pos = "nouns",                 # utilizing nouns specifically for the analysis
                         remove_stop_words = TRUE,      # remove stop words from the data
                         compound_nouns = TRUE)         # compare nouns against each other

sotu_text_network <- CreateTextnet(prepped_sotu)        # convert the matrix to a text network     

VisTextNet(sotu_text_network, label_degree_cut = 0)     # visualize the text network

VisTextNetD3(sotu_text_network)                         # use 3d visualization of the textnet

library(htmlwidgets)                                    # loading the widgets library 

vis <- VisTextNetD3(sotu_text_network,                  # Use 3d visualization of the textnet
                    height=1000,                        # Set max height to 1000 units
                    width=1400,                         # set max weidth to 1400 units 
                    bound=FALSE,                        # Don't allow the vis to move
                    zoom=TRUE,                          # allow zooming
                    charge=-30)                         # 

saveWidget(vis, "sotu_textnet.html")

sotu_communities <- TextCommunities(sotu_text_network)

head(sotu_communities)

top_words_modularity_classes <- InterpretText(sotu_text_network, prepped_sotu)
head(top_words_modularity_classes)

text_centrality <- TextCentrality(sotu_text_network)

