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
Node *build(Node *left, Node *right);
void freeTree(Node *root);
Node *copyTree(Node *root);
Node *takeCons(Node *left, Node *right);
Node *takeHead(Node *root);
Node *takeTail(Node *root);
bool treeEqual(Node* a, Node *b);
Node **setUpVars(int maxVarIdx);

#endif
