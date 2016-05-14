/**
 * @author Adrian Rodriguez Bazaga, Eleazar Diaz Delgado
 * @version 1.0.0
 * @date 19 May 2016
 * @email Adrián: alu0100826456@ull.edu.es / arodriba@ull.edu.es | Eleazar: eleazardzdo@gmail.com
 * @subject Inteligencia Artificial Avanzada (Advanced Artificial Intelligence)
 * @title Assignment 6 - Classification using Natural Language Processing
 */

package logic;

import helpers.FileParser;

public class Main {
  public static void main(String[] args) {
    // Create the classification object
    Classifier classifier = new Classifier();
    // Create the initial corpus (duplicated texts included)
    classifier.setCorpus(FileParser.parseCorpus("C:\\Users\\QiCanarias23\\workspace\\es.ull.esit.iaa.pln.clasificacion\\resources\\corpustodo.txt"));
    // Create the unique corpus (removing duplicated texts)
    classifier.createUniqueCorpus();
    
    //classifier.showUniqueCorpus();
    //System.out.println(classifier.getUniqueCorpusSize());
    classifier.getRelProbOfWords();     // Get the relevant probabilities of every word in the vocabulary
    classifier.getNrelProbOfWords();    // Get the not relevant probabilities of every word in the vocabulary
    classifier.classifyText();          // Classify every corpus text as relevant or not relevant using the previously calculated probabilities
  }
}
