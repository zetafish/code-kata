// http://www.spoj.pl/problems/PHONELST/
// 
// Plain C implementation

#include <stdio.h>

#define DIGIT(ch) ((ch) - '0')

struct node {
  struct node* child[10];
  char ch;
  char last;
};

struct node NODES[10000 * 10];
struct node* root[10];
struct node* node_ptr;
int consistent;

struct node* create_node(char ch) {
  struct node* node = node_ptr++;

  //printf("create_node for %c\n", ch);
  memset(node, 0, sizeof(*node));
  node->ch = ch;
  return node;
};

char* to_s(struct node* node) {
  static char buf[] = "...........[.]";
  int i;
  for (i = 0; i<10 ; ++i) {
    buf[i] =  (node->child[i]) ? '0' + i : '.';
  }
  buf[10] = node->last? 'T' : 'c';
  buf[12] = node->ch;
  return buf;
}

struct node* create_trie(char* s) {
  struct node* node;
  struct node* p;

  p = node = create_node(*s);
  while (*++s) {
    char ch = *s;
    int n = DIGIT(ch);
    p = p->child[n] = create_node(ch);
  }
  p->last = 1;
  return node;
}

struct node* ins(struct node* node, char* s){
  if (node == NULL) {
    node = create_trie(s);
  }
  else if (node->last) {
    consistent = 0;
  }
  else if (consistent) {
    char ch = *(s+1);
    int n = DIGIT(ch);
    node->child[n] = ins(node->child[n], s+1);
  }
  return node;
}


int read_number()
{
  char s[10];
  return atoi(gets(s));
}

void dump_roots() {
  int i;
  for (i=0 ; i<10 ; ++i) {
    printf("root[%d]=%s\n", i, root[i] ? to_s(root[i]) : "NULL");
  }
}

void solve() {
  int n = read_number();
  char s[20];
  
  memset(root, 0, sizeof(root));
  node_ptr = NODES;
  consistent = 1;
  while (n-- > 0) {
    gets(s);
    if (consistent) {
      int n = DIGIT(s[0]);
      root[n] = ins(root[n], s); 
    }
  }
  puts(consistent ? "YES" : "NO");
}

int main(int argc, char** argv) {
  int n = read_number();
  while (n-- > 0) {
    solve();
  }
  return 0;
}

