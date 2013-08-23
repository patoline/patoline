#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <string.h>
#include <unistd.h>
#include <gnutls/gnutls.h>
#include <gcrypt.h>
#include <pthread.h>

#define KEYFILE "server.key"
#define CERTFILE "server.crt"
#define CAFILE "/etc/ssl/certs/ca-certificates.crt"
#define CRLFILE "crl.pem"

#define MAX_BUF 1024
#define PORT 5556               /* listen to 5556 port */
#define SPORT 5557              /* listen to 5556 port */

/* These are global */
static gnutls_dh_params_t dh_params;

static int
generate_dh_params (void)
{
  unsigned int bits =
    gnutls_sec_param_to_pk_bits (GNUTLS_PK_DH, GNUTLS_SEC_PARAM_LEGACY);

  /* Generate Diffie-Hellman parameters - for use with DHE
   * kx algorithms. When short bit length is used, it might
   * be wise to regenerate parameters often.
   */
  gnutls_dh_params_init (&dh_params);
  gnutls_dh_params_generate2 (dh_params, bits);

  return 0;
}

struct conn {
  int sd;
  int use_tls;
  gnutls_session_t session;
};
void *answer(void*);
int base64_encode(unsigned char*,unsigned char*,int);
void do_send(struct conn*conn,void*data,int len);
int websocket_send(struct conn*conn,char*data,int len);


struct client_tree {
  struct conn* conn;
  unsigned int card;
  struct client_tree* right;
  struct client_tree* left;
};
struct client_tree* insert(struct client_tree* tree,struct conn* conn);
void send_to_all(struct client_tree* t, void* data,int len);

void free_conn(struct conn* conn){
  if(conn->use_tls) gnutls_bye (conn->session, GNUTLS_SHUT_WR);
  close (conn->sd);
  if(conn->use_tls) gnutls_deinit (conn->session);
  free(conn);
}

int current_slide;
int current_state;
struct client_tree* clients=NULL;
pthread_mutex_t m_state;

int main (void)
{
  int listen_sd;
  int listen_sd_s;
  int ret;
  gnutls_certificate_credentials_t x509_cred;
  gnutls_priority_t priority_cache;
  struct sockaddr_in sa_serv;
  struct sockaddr_in sa_serv_s;
  gnutls_session_t session;
  int optval = 1;
  fd_set fds;
  pthread_t thr;
  struct conn*conn;

  /* this must be called once in the program
   */
  gnutls_global_init ();
  gnutls_certificate_allocate_credentials (&x509_cred);
  /* gnutls_certificate_set_x509_system_trust(xcred); */
  gnutls_certificate_set_x509_trust_file (x509_cred, CAFILE,
                                          GNUTLS_X509_FMT_PEM);
  gnutls_certificate_set_x509_crl_file (x509_cred, CRLFILE,
                                        GNUTLS_X509_FMT_PEM);
  ret = gnutls_certificate_set_x509_key_file (x509_cred, CERTFILE, KEYFILE,
                                        GNUTLS_X509_FMT_PEM);
  if (ret < 0)
    {
      printf("No certificate or key were found\nTo generate one, use:\n\n");
      printf("openssl genrsa -des3 -out server.key 1024\n");
      printf("openssl req -new -key server.key -out server.csr\n");
      printf("cp server.key server.key.org\n");
      printf("openssl rsa -in server.key.org -out server.key\n");
      printf("openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt\n");
      exit(1);
    }
  generate_dh_params ();
  gnutls_priority_init (&priority_cache, "PERFORMANCE:%SERVER_PRECEDENCE", NULL);
  gnutls_certificate_set_dh_params (x509_cred, dh_params);

  /* Socket operations */

  listen_sd = socket (AF_INET, SOCK_STREAM, 0);
  listen_sd_s = socket (AF_INET, SOCK_STREAM, 0);

  memset (&sa_serv, '\0', sizeof (sa_serv));
  sa_serv.sin_family = AF_INET;
  sa_serv.sin_addr.s_addr = INADDR_ANY;
  sa_serv.sin_port = htons (PORT);      /* Server Port number */

  memset (&sa_serv_s, '\0', sizeof (sa_serv_s));
  sa_serv_s.sin_family = AF_INET;
  sa_serv_s.sin_addr.s_addr = INADDR_ANY;
  sa_serv_s.sin_port = htons (SPORT);      /* Server Port number */


  setsockopt (listen_sd, SOL_SOCKET, SO_REUSEADDR, (void *) &optval,
              sizeof (int));
  setsockopt (listen_sd_s, SOL_SOCKET, SO_REUSEADDR, (void *) &optval,
              sizeof (int));

  bind (listen_sd, (struct sockaddr *) & sa_serv, sizeof (sa_serv));
  bind (listen_sd_s, (struct sockaddr *) & sa_serv_s, sizeof (sa_serv_s));
  listen (listen_sd, 1024);
  listen (listen_sd_s, 1024);

  pthread_mutex_init(&m_state, NULL);

  for (;;)
    {
      gnutls_init (&session, GNUTLS_SERVER);

      gnutls_priority_set (session, priority_cache);
      gnutls_credentials_set (session, GNUTLS_CRD_CERTIFICATE, x509_cred);
      /* We don't request any certificate from the client.
       * If we did we would need to verify it. */
      gnutls_certificate_server_set_request (session, GNUTLS_CERT_IGNORE);

      printf ("Server ready. Listening to port '%d' and '%d'.\n\n", PORT, SPORT);

      FD_ZERO(&fds);
      FD_SET(listen_sd,&fds);
      FD_SET(listen_sd_s,&fds);
      printf("select\n");
      select(listen_sd>listen_sd_s?(1+listen_sd):(1+listen_sd_s),&fds,NULL,NULL,0);
      printf("/select\n");

      if(FD_ISSET(listen_sd_s,&fds)) {
        conn=malloc(sizeof(struct conn));
        conn->sd=accept (listen_sd_s,NULL,NULL);
        conn->use_tls=1;
        conn->session=session;
        printf("https %d\n", conn->sd);
        /*printf ("- connection from %s, port %d\n",
                inet_ntop (AF_INET, &sa_cli.sin_addr, topbuf,
                           sizeof (topbuf)), ntohs (sa_cli.sin_port));
        */
        pthread_create(&thr,NULL,&answer,conn);

      } else {

        conn=malloc(sizeof(struct conn));
        conn->sd=accept (listen_sd, NULL,NULL);
        conn->use_tls=0;
        printf("http %d\n",conn->sd);
        /*printf ("- connection from %s, port %d\n",
          inet_ntop (AF_INET, &sa_cli.sin_addr, topbuf,
          sizeof (topbuf)), ntohs (sa_cli.sin_port));
        */
        pthread_create(&thr,NULL,&answer,conn);
      }
    }
  close (listen_sd);
  close (listen_sd_s);
  gnutls_certificate_free_credentials (x509_cred);
  gnutls_priority_deinit (priority_cache);
  gnutls_global_deinit ();
  return 0;

}


void do_send(struct conn*conn,void*data,int len){
  if(conn->use_tls)
    gnutls_record_send(conn->session, data, len);
  else {
    printf("send conn %d\n",conn->sd);
    write(conn->sd,data,len);
  }
}

void *answer(void *conn_){
  // Un premier truc à faire: vérifier qu'on n'a pas déjà conn->fd
  // dans l'arbre, et le libérer le cas échéant. Ça évitera bien des conneries.
  struct conn*conn=conn_;
  int ret;

  char* buffer=malloc(MAX_BUF);
  size_t line_size=MAX_BUF;
  char* line_buf=malloc(line_size);
  unsigned int line_b=0;
  char* req=NULL;
  int req_is_valid=0;
  char* line_off=line_buf;
  unsigned char* key_hashed=NULL;

  int keep_alive=1;
  unsigned char* websocket_answer=NULL;

  if(conn->use_tls){

    gnutls_transport_set_int (conn->session, conn->sd);
    do
      {
        ret = gnutls_handshake (conn->session);
      }
    while (ret < 0 && gnutls_error_is_fatal (ret) == 0);

    if (ret < 0)
      {
        close (conn->sd);
        gnutls_deinit (conn->session);
        fprintf (stderr, "*** Handshake has failed (%s)\n\n",
                 gnutls_strerror (ret));
        free(conn);
        pthread_exit (NULL);
      }
    printf ("- Handshake was completed\n");
  }
  /* see the Getting peer's information example */
  /* print_info(session); */

  /* lecture de la requête. Comme il n'y a pas de readline avec tls,
     c'est un peu tordu. Il y a un buffer de lecture, et un autre
     buffer "de ligne". Quand on n'a plus assez de truc dans le buffer
     de ligne pour voir CRLF, on fait need_more=1, et ça lit des trucs
     en plus. Dès qu'on a vu CRLFCRLF, on s'arrête et on traite la
     requête.

     line_buf: tout ce qu'on a lu depuis la dernière fois qu'on a vu un CRLF.

     line_off: les trucs qui restent à traiter (line_off0 est une
     marque pour remettre line_off où il faut) */


  int is_websocket=0;
  while(keep_alive){
    int need_more=1;
    char* websocket_key=NULL;
    int websocket_len=0;
    while(1)
      {
        if(need_more){
          if(conn->use_tls) {
            ret = gnutls_record_recv (conn->session, buffer, MAX_BUF);
          } else {
            ret=read(conn->sd,buffer,MAX_BUF);
          }
          need_more=0;
          if (ret == 0) {
            printf ("\n- Peer has closed the GnuTLS connection\n");
            keep_alive=0;
            break;
          } else if (conn->use_tls && ret < 0 && gnutls_error_is_fatal (ret) == 0) {
            fprintf (stderr, "*** Warning: %s\n", gnutls_strerror (ret));
          } else if (ret < 0) {
            fprintf (stderr, "\n*** Received corrupted "
                     "data(%d). Closing the connection.\n\n", ret);
            keep_alive=0;
            break;
          } else if(ret+line_b >= MAX_BUF) {
            fprintf(stderr, "\n*** Request too long\n\n");
            keep_alive=0;
            break;
          } else {
            memcpy(line_buf+line_b, buffer, (ret+line_b)>=MAX_BUF?MAX_BUF:ret);
            line_b+=ret;
            line_buf[line_b]='\0';
          }
        }
        if(ret>0){
          if(strcmp(line_off,"\r\n")==0){ /* ligne vide, line_off est nul-terminated */
            break;
          } else {
            /* on cherche le prochain CRLF */
            char* line_off0=line_off;
            line_off=strstr(line_off,"\r\n");
            if(line_off){ /* trouvé. */
              line_off[0]='\0';
              if(strncmp(line_off0,"GET",3)==0){
                int i=3;
                int j;
                printf("GET\n");
                while(line_off0[i]==' ' && line_off0[i]) i++;
                j=i;
                while(line_off0[j]!=' ' && line_off0[i]) j++;
                req=realloc(req,j-i+1);
                req_is_valid=1;
                memcpy(req,line_off0+i,j-i);
                req[j-i]='\0';
              } else {
                if(req_is_valid){
                  if(strcmp(req,"/tire")==0){
                    char*hdr="Sec-WebSocket-Key";
                    if(strncmp(line_off0,hdr,strlen(hdr))==0) {
                      int i=0;
                      while(line_off0[i]!=' ' && line_off0[i]) i++;
                      while(line_off0[i]==' ' && line_off0[i]) i++;
                      websocket_len=line_off-line_off0-i;
                      websocket_key=realloc(websocket_key,websocket_len);
                      memcpy(websocket_key, line_off0+i, websocket_len);
                      printf("header: \"%s\"\n",line_off0);
                    } else
                      printf("header: \"%s\"\n",line_off0);
                  } else {
                    printf("header: %s\n",line_off0);
                  }
                } else {
                  printf("unsupported request %s\n",line_off0);
                }
              }
              need_more=0;
              line_off+=2;
            } else { /* on veut lire plus. */
              need_more=1;
              line_b=line_b-(line_off0-line_buf);
              line_off=line_buf;
              if(line_off0!=line_buf){
                memmove(line_buf,line_off0,line_b);
              }
            }
          }
        }
      }
    line_b=0;
    /* reponse */
    req_is_valid=0;
    if(req){
      printf ("req terminée \"%s\"\n",req);
      if(strcmp(req,"/tire")==0){
        char* magic="258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
        char* fin="\r\n\r\n";
        char*ans="HTTP/1.1 101 Switching\r\nUpgrade: websocket\r\n"
          "Connection: upgrade\r\nSec-WebSocket-Accept: ";

        int hash_length0=gcry_md_get_algo_dlen(GCRY_MD_SHA1);
        int hash_length=hash_length0;
        unsigned char *hash;
        int l;
        hash_length=(hash_length+2)/3;
        hash=malloc(hash_length*3);
        memset(hash,0,hash_length*3);

        do_send(conn,ans,strlen(ans));

        websocket_answer=realloc(websocket_answer,websocket_len+strlen(magic));
        memcpy(websocket_answer,websocket_key,websocket_len);
        memcpy(websocket_answer+websocket_len,magic,strlen(magic));

        gcry_md_hash_buffer(GCRY_MD_SHA1, hash, websocket_answer, websocket_len + strlen(magic));

        key_hashed=realloc(key_hashed, hash_length*4);
        l=base64_encode(key_hashed,hash,hash_length0);
        free(hash);

        do_send(conn,key_hashed,l);
        do_send(conn,fin,strlen(fin));

        char* state;
        asprintf(&state,"{ \"slide\":%d, \"state\":%d }",0,0);
        websocket_send(conn,state,strlen(state));
        free(state);

        pthread_mutex_lock(&m_state);
        clients=insert(clients,conn);
        pthread_mutex_unlock(&m_state);
        is_websocket=1;
        break;

      } else if(strcmp(req,"/")==0) {

        char*repl=NULL;
        asprintf(&repl,"HTTP/1.1 200 Ok\r\nContent-Type: text/html\r\nContent-Length: %d\r\n\r\n",(int)strlen(page));
        do_send(conn,repl,strlen(repl));
        do_send(conn,page,strlen(page));
        free(repl);

      } else if(strncmp(req,"/pousse",strlen("/pousse"))==0) {

        pthread_mutex_lock(&m_state);
        char* end;
        current_slide=strtol(req+strlen("/pousse")+1,&end,10);
        current_state=strtol(end+1,&end,10);

        char* state;
        asprintf(&state,"{ \"slide\":%d, \"state\":%d }",current_slide,current_state);

        send_to_all(clients,state,strlen(state));

        free(state);
        pthread_mutex_unlock(&m_state);
        printf("conn %p\n",conn);
        char*repl=NULL;
        char*page="ok\r\n";
        asprintf(&repl,"HTTP/1.1 200 Ok\r\nContent-Type: text/html\r\nContent-Length: %d\r\n\r\n%s",(int)strlen(page),page);
        do_send(conn,repl,strlen(repl));
        free(repl);

      } else if(strcmp(req,"/style.css")==0){

        char*repl=NULL;
        asprintf(&repl,"HTTP/1.1 200 Ok\r\nContent-Type: text/css\r\nContent-Length: %d\r\n\r\n",(int)strlen(css));
        do_send(conn,repl,strlen(repl));
        do_send(conn,css,strlen(css));
        free(repl);

      } else {

        // extension
        int i=0;
        while(req[i] && req[i]!='.') i++;
        if(strcmp(req+i,".svg")==0){

          /* svg */
          char* end;
          int slide=strtol(req+1,&end,10);
          int state=strtol(end+1,&end,10);
          char* repl;
          asprintf(&repl,"HTTP/1.1 200 Ok\r\nContent-Type: image/svg+xml\r\nContent-Length: %d\r\n\r\n",strlen_slides[slide][state]);
          do_send(conn,repl,strlen(repl));
          do_send(conn,slides[slide][state],strlen_slides[slide][state]);
          free(repl);

        } else {

          /* binary data */
          i=0;
          while(i<n_bin && strcmp(req,bin[i][0]+strlen(bin[i][0]) - strlen(req))){
            printf("->%s\n",bin[i][0]);
            i++;
          }
          if(i<n_bin){
            char* repl;
            asprintf(&repl,"HTTP/1.1 200 Ok\r\nContent-Type: font/opentype\r\nContent-Length: %d\r\n\r\n",strlen_bin[i][1]);
            do_send(conn,repl,strlen(repl));
            do_send(conn,bin[i][1],strlen_bin[i][1]);
            do_send(conn,"\r\n",strlen("\r\n"));
            free(repl);
          } else {
            char*resp="HTTP/1.1 404 Not found\r\nContent-Type: text/html;charset=utf-8\r\n\r\n<html><body><h1>Page non trouvée</h1></body></html>";
            do_send(conn,resp,strlen(resp));
          }
        }
      }
      keep_alive=0;
    }
    /* do not wait for the peer to close the connection. */
  }
  if(!is_websocket)
    free_conn(conn);
  free(websocket_answer);
  free(buffer);
  free(line_buf);
  free(req);
  pthread_exit(NULL);
}

unsigned char value_base64(unsigned char x);
unsigned char value_base64(unsigned char x){
  unsigned char y=x & 0x3f;
  if(y<26) return (y+'A'); else
  if(y<52) return (y-26+'a'); else
  if(y<62) return (y-52+'0'); else
  if(y==62) return ('+'); else
    return '/';
}

int base64_encode(unsigned char* s1,unsigned char* s2,int n){
  unsigned int i=0,j=0;
  unsigned int m=((n+2)/3)*3;
  while(i<m-2){
    unsigned int a=s2[i], b=s2[i+1], c=s2[i+2];
    unsigned int x=(((a << 8) | b) << 8) | c;
    s1[j]=value_base64(x>>18);
    s1[j+1]=value_base64(x>>12);
    s1[j+2]=value_base64(x>>6);
    s1[j+3]=value_base64(x);
    j+=4;
    i+=3;
  }
  if(n%3 >= 1)
    s1[j-1]='=';
  if(n%3 == 1)
    s1[j-2]='=';
  s1[j]=0;
  return j;
}

#define PACKET_LEN 256

int min(int a,int b){
  if(a<=b) return a; else return b;
}

int websocket_send(struct conn*conn,char* data,int len){
  char packet[PACKET_LEN+10];
  int pos=0;
  while(pos<len){
    int i=0;
    int fin=len<(pos+PACKET_LEN) ? 0x80:0;
    int opcode=pos==0?1:0;
    int rsv1=0,rsv2=0,rsv3=0;
    unsigned long int payload_len=min(len-pos,PACKET_LEN);
    int pos_max;
    packet[0]=fin | rsv1 | rsv2 | rsv3 | opcode;
    if(payload_len<=125){
      packet[1]=(char) payload_len;
      i=2;
    } else if(payload_len<=0xffff) {
      packet[1]=126;
      packet[2]=(char) (payload_len >> 8);
      packet[3]=(char) (payload_len&0xff);
      i=4;
    } else {
      packet[1]=127;
      packet[2]=(char) ((payload_len >> 56) & 0xff);
      packet[3]=(char) ((payload_len >> 48) & 0xff);
      packet[4]=(char) ((payload_len >> 32) & 0xff);
      packet[5]=(char) ((payload_len >> 24) & 0xff);
      packet[6]=(char) ((payload_len >> 16) & 0xff);
      packet[7]=(char) ((payload_len >> 8) & 0xff);
      packet[8]=(char) (payload_len & 0xff);
      i=9;
    }
    pos_max=pos+payload_len;
    for(;pos<pos_max;pos++){
      packet[i]=data[pos];
      i++;
    }
    packet[i]=0;
    printf("websocket_send %s\n",packet);
    do_send(conn,packet,i);
    printf("/websocket_send\n");
  }
  return 0;
}



struct client_tree* insert(struct client_tree* tree,struct conn* conn){
  if(tree){
    tree->card++;
    if(conn->sd < tree->conn->sd){
      insert(tree->left,conn);
    } else if(conn->sd > tree->conn->sd){
      insert(tree->right, conn);
    } else {
      free_conn(tree->conn);
      tree->conn=conn;
    }

    if(tree->left && tree->right) {
      if(tree->left->card > tree->right->card + 10){
        struct client_tree* tmp=tree->left->right;
        tree->left->right=tree;
        tree->left=tmp;
        return tree->left;
      } else if(tree->left->card+10 < tree->right->card) {
        struct client_tree* tmp=tree->right->left;
        tree->right->left=tree;
        tree->right=tmp;
        return tree->right;
      } else {
        return tree;
      }
    } else {
      return tree;
    }
  } else {

    struct client_tree* t=malloc(sizeof(struct client_tree*));
    t->right=NULL;
    t->left=NULL;
    t->card=1;
    t->conn=conn;
    return t;

  }
}

void send_to_all(struct client_tree* t, void* data,int len){
  if(t){
    printf("to_all: %d\n",t->conn->sd);
    websocket_send(t->conn,data,len);
    send_to_all(t->left,data,len);
    send_to_all(t->right,data,len);
  }
}
