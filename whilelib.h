/*
 * Header file for whilelib.c.
 * Contains data definitions for data types used by compiled while programs.
 */

#ifndef WHILELIB
#define WHILELIB

typedef enum {
    nil,
    cons
} NodeType;

typedef enum {
    false,
    true
} bool;

typedef struct Node Node;
struct Node {
    NodeType nodeType;
    Node *left;
    Node *right;
};

void *ckMalloc(int size);
Node *newNil();
Node *newCons(Node *left, Node *right);
void freeTree(Node *root);
Node *copyTree(Node *root);
Node *takeHead(Node *root);
Node *takeTail(Node *root);
Node *doEquals(Node *a, Node *b);
bool treeEqual(Node *a, Node *b);
Node **setUpStore(int maxVarIdx);
void freeStore(int maxVarIdx, Node **store);
void printTreeLinear(Node *node);

#endif
