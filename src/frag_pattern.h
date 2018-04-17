#ifndef FRAG_PATTERN_H
#define FRAG_PATTERN_H

#include <iostream>
#include <fstream>

#include <boost/graph/graphml.hpp>

#include "common.h"
#include "k_path_tree.h"
#include "triangles_list.h"
#include "adjacencyGraph.h"

class adjacencyGraph;
class k_path_tree;

class frag_pattern
{
    public:
        frag_pattern();

        //Constructor which construct a single edge pattern.
        frag_pattern(Vertext v,k_path_tree& kt,std::ostream& os);

        //Constructor which extend a fragmentation pattern.
        frag_pattern(frag_pattern& fp,k_path_tree& kt,int iext,
                      int fthreshold,bool& created,std::ostream& os);

        //To do implment this constructore.
        //frag_pattern(k_path_tree& kt,Vertext v,triangles_list& tl, bool& created);
        virtual ~frag_pattern();
        graphp& get_g();
        patternKey get_key() const;
        std::vector<occ>& get_occs();

        //The normal form is just composed of the edge comming directly form 0.
        void calcNormalForm();

        Vertexp get_root();

        //Remove the part of pattern used for extension.
        void clearPattern();

        int numExts();
        int numOccs();
        int sizeGraph();
        void clearExts();

        //TO impement later
        void extendMotif();

        std::pair<std::vector<Extension>::iterator,
                    std::vector<Extension>::iterator> get_it_exts();

        void constructFullGraph(triangles_list& tl);
        void constructFullGraph(triangles_list& tl,Vertexp v);

        //Function to debug only
        bool isCoherent();

        std::string& get_norm();


        //These function are implmented in the frag_pattern class closed.
        std::vector<Edgep> enumerate_possible_subpattern();
        std::vector<short> get_out_edge_labs(Edgep e);

        //Utility function
        void to_string();
        void to_reduced_string();
        void to_reduced_string(std::ostream& of);
        void to_reduced_string_ext(std::ostream& of);
        std::set<short> numUniqueOccs();

        void write_graphml(std::string path_graphml);

        void clearPatternFull();

        bool null=false;

    protected:
        //extensions is in left-mostg depth-first.
        std::vector<Extension> extensions;

    private:
        //Attributes used for extension.
        //See if recalculating it speed up the process.
        std::unordered_set<short> dist_prec;
        std::vector<occ> occurences;

        //This key is used for indexing by the data structure.
        patternKey key;

        //
        std::string norm;

        //The attribute store the graph
        graphp g;

        //A pointer to the root for commodity purpose
        Vertexp root;

        bool validExtension(Extension& ext,triangles_list& l);
        void fill_graph_info();
        //Remove the forbidden extensions.
        void filterExtensions(triangles_list&);
        std::vector<Extension>::iterator update_dist_prec(std::vector<Extension>&,Extension,triangles_list&);

};

//These function define the isomorphisms, the majority of these
//When it is a subgraph calculation, a is always supposed to be the subgraph.
Vertexp find_subgraph(frag_pattern & a, frag_pattern & b);
bool is_isomorphic(frag_pattern & a, frag_pattern & b);
bool is_isomorphic(frag_pattern & a,std::vector<short> & b);
bool is_subgraph(frag_pattern & a, frag_pattern & b, Vertexp v);
bool is_subgraph(frag_pattern & a, frag_pattern & b);
bool is_subgraph(frag_pattern & a, frag_pattern & b,std::ofstream& of);
bool is_subgraph_edge_set(std::set<short> elabs,frag_pattern & p);
#endif // FRAG_PATTERN_H