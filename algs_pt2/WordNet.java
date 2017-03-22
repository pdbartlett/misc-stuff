import edu.princeton.cs.algs4.Digraph;
import edu.princeton.cs.algs4.In;

import java.util.HashMap;
import java.util.Map;

public class WordNet {
  private Map<String, Integer> nounIndices = new HashMap<>();
  private String[] synsets;
  private Digraph dag;
  private SAP sap;

  // constructor takes the name of the two input files
  public WordNet(String synsets, String hypernyms) {
    int index = 0;
    String[] lines = new In(synsets).readAllLines();
    this.synsets = new String[lines.length];
    for (String line : lines) {
      String[] parts = line.split(",");
      if (index != Integer.parseInt(parts[0])) {
        throw new RuntimeException("Unexpected index: " + parts[0]);
      }
      this.synsets[index] = parts[1];
      for (String noun : parts[1].split(" ")) {
        nounIndices.put(noun, index);
      }
      ++index;
    }

    dag = new Digraph(this.synsets.length);
    for (String line : new In(hypernyms).readAllLines()) {
      String[] parts = line.split(",");
      int nounIndex = Integer.parseInt(parts[0]);
      for (int i = 1; i < parts.length; ++i) {
        dag.addEdge(nounIndex, Integer.parseInt(parts[i]));
      }
    }

    sap = new SAP(dag);
  }

  // returns all WordNet nouns
  public Iterable<String> nouns() {
    return nounIndices.keySet();
  }

  // is the word a WordNet noun?
  public boolean isNoun(String word) {
    return nounIndices.containsKey(word);
  }

  // distance between nounA and nounB (defined below)
  public int distance(String nounA, String nounB) {
    return sap.length(nounIndices.get(nounA), nounIndices.get(nounB));
  }

  // a synset (second field of synsets.txt) that is the common ancestor of nounA and nounB
  // in a shortest ancestral path (defined below)
  public String sap(String nounA, String nounB) {
    int index = sap.ancestor(nounIndices.get(nounA), nounIndices.get(nounB));
    return synsets[index];
  }

  // do unit testing of this class
  public static void main(String[] args) { }
}
