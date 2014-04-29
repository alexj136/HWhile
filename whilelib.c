#include <stdio.h>
#include <malloc.h>
#include <assert.h>
#include "whilelib.h"

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void *ckMalloc(int size) {
    void *ptr = malloc(size);
    assert(ptr);
    return ptr;
}

/*
 * Create a new nil node on the heap, return a pointer to it.
 */
Node *newNil() {
    Node *nilNode = ckMalloc(sizeof(Node));
    nilNode->nodeType = nil;
    return nilNode;
}

/*
 * Build a cons node from a left and right subtree. The given trees are part of
 * the returned trees - they are not copied, so freeing a tree given as an
 * argument to this function will cause crazy errors. Don't do it.
 */
Node *build(Node *left, Node *right) {
    Node *consNode = ckMalloc(sizeof(Node));
    consNode->nodeType = cons;
    consNode->left = left;
    consNode->right = right;
    return consNode;
}

/*
 * Free an entire tree.
 */
void freeTree(Node *root) {
    if(root->nodeType == cons) {
        freeTree(root->left);
        freeTree(root->right);
    }
    else {
        assert(root->nodeType == nil);
    }
    free(root);
    return;
}

/*
 * Make a copy of the given tree on the heap.
 */
Node *copyTree(Node *root) {
    if(root->nodeType == nil) {
        return newNil();
    }
    else {
        return build(copyTree(root->left), copyTree(root->right));
    }
}

/*
 * Build an entirely new tree that is the cons of the two given trees. The given
 * subtrees can be safely freed without affecting the returned tree.
 */
Node *takeCons(Node *left, Node *right) {
    return build(copyTree(left), copyTree(right));
}

/*
 * Build an entirely new tree that is the head of the given tree. The given tree
 * can be safely freed without affecting the returned tree.
 */
Node *takeHead(Node *root) {
    if(root->nodeType == nil) {
        return newNil();
    }
    else {
        assert(root->nodeType == cons);
        return copyTree(root->left);
    }
}

/*
 * Build an entirely new tree that is the tail of the given tree. The given tree
 * can be safely freed without affecting the returned tree.
 */
Node *takeTail(Node *root) {
    if(root->nodeType == nil) {
        return newNil();
    }
    else {
        assert(root->nodeType == cons);
        return copyTree(root->right);
    }
}

/*
 * Determine whether or not two trees have the exact same structure.
 */
bool treeEqual(Node* a, Node *b) {
    if(a->nodeType == nil && b->nodeType == nil) {
        return true;
    }
    else {
        return treeEqual(a->left, b->left) && treeEqual(a->right, b->right);
    }
}

/*
 * Set up the array containing the variables
 */
Node **setUpVars(int maxVarIdx) {
    Node **varArr = ckMalloc((maxVarIdx + 1) * sizeof(Node*));
    int i;
    for(i = 0; i <= maxVarIdx; i++) {
        varArr[i] = newNil();
    }
    return varArr;
}
