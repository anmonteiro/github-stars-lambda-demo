open Lwt.Infix;

module Html = Cow.Html;
module Json = {
  include Yojson.Safe.Util;

  let member_opt = (m, t) =>
    try (Some(member(m, t))) {
    | _ => None
    };
};

type stargazer = {
  login: string,
  name: option(string),
  avatar: string,
};

type starsInfo = {
  count: int,
  stargazers: list(stargazer),
};

type repo = {
  nameWithOwner: string,
  stars: starsInfo,
};

module MyQuery = [%graphql
  {|
query ($name: String!, $owner: String!, $first: Int!, $token: String!) {
  gitHub(
    auths: {
      gitHubOAuthToken: $token
    }
  ) {
    repository(
      name: $name
      owner: $owner
    ) {
      id
      nameWithOwner
      stargazers(
        orderBy: { direction: DESC, field: STARRED_AT }
        first: $first
      ) {
        edges {
          node {
            id
            login
            avatarUrl
            name
          }
        }
        totalCount
      }
    }
  }
}
|}
];

let send_response = body => {
  Lwt_result.return @@
  Now_lambda.{
    status_code: 200,
    headers:
      Lambda_runtime.StringMap.(empty |> add("content-type", "text/html")),
    body,
    encoding: None,
  };
};

let send_error_response = msg => {
  Lwt_result.return @@
  Now_lambda.{
    status_code: 400,
    headers:
      Lambda_runtime.StringMap.(empty |> add("content-type", "text/html")),
    body: msg,
    encoding: None,
  };
};

let getGraphQLResponse = (~first=?, ~name, ~owner) => {
  let first =
    switch (first) {
    | None => 10
    | Some(s) =>
      switch (int_of_string_opt(s)) {
      | Some(n) => n
      | None => 10
      }
    };
  let q =
    MyQuery.make(
      ~name,
      ~owner,
      ~first,
      ~token=Request.private_github_token,
      (),
    );
  let queryJson =
    `Assoc([("query", `String(q#query)), ("variables", q#variables)]);
  let reqBody = Yojson.Safe.to_string(queryJson);
  let uri = "https://serve.onegraph.com/dynamic?app_id=eaf48ccf-cb5b-4a5b-866e-d223e60cfea7";
  Request.send(
    uri,
    ~meth=`POST,
    ~body=reqBody,
    ~additional_headers=[
      ("content-length", string_of_int(String.length(reqBody))),
    ],
  )
  >|= (
    res => {
      let json_result = Yojson.Safe.from_string(res);
      switch (Json.member_opt("data", json_result)) {
      | Some(data) =>
        let data = q#parse(data);
        switch (data#gitHub) {
        | Some(github) =>
          switch (github#repository) {
          | Some(repo) =>
            let starsInfo = repo#stargazers;
            let starsCount = starsInfo#totalCount;
            switch (starsInfo#edges) {
            | Some(stargazers) =>
              let stargazers =
                Array.fold_left(
                  acc =>
                    fun
                    | None => acc
                    | Some(i) => {
                        let node = i#node;
                        [
                          {
                            name: node#name,
                            login: node#login,
                            avatar: node#avatarUrl,
                          },
                          ...acc,
                        ];
                      },
                  [],
                  stargazers,
                );
              Ok({
                nameWithOwner: repo#nameWithOwner,
                stars: {
                  count: starsCount,
                  stargazers: List.rev(stargazers),
                },
              });
            | None => Error("There was an error getting your data.")
            };
          | None => Error("There was an error getting your data.")
          }
        | None => Error("There was an error getting your data.")
        };
      | None => Error("There was an error getting your data.")
      };
    }
  );
};

let handle_result =
  fun
  | Error(msg) => send_error_response(msg)
  | Ok({nameWithOwner, stars: {count, stargazers}, _}) => {
      let buf = Buffer.create(100);
      Html.(
        output_doc(
          `Buffer(buf),
          html(
            list([
              head(meta(~charset="UTF-8", [])),
              body(
                list([
                  h1(string("LIVE FROM REASON CONF")),
                  h1(
                    string(
                      Format.asprintf(
                        "Stargazer summary for: %s",
                        nameWithOwner,
                      ),
                    ),
                  ),
                  h2(
                    string(
                      Format.asprintf("Total number of stars: %d", count),
                    ),
                  ),
                  list(
                    List.map(
                      ({avatar, login, name}) =>
                        div(
                          ~attrs=[
                            ("style", "display:flex;align-items:center"),
                          ],
                          list([
                            img(~height=100, Uri.of_string(avatar)),
                            span(
                              ~attrs=[("style", "margin-left:20px")],
                              string(
                                switch (name) {
                                | None
                                | Some("") => login
                                | Some(name) =>
                                  Format.asprintf("%s (%s)", name, login)
                                },
                              ),
                            ),
                          ]),
                        ),
                      stargazers,
                    ),
                  ),
                ]),
              ),
            ]),
          ),
        )
      );
      send_response(Buffer.contents(buf));
    };

let handle = (~first=?, ~name, ~owner) => {
  getGraphQLResponse(~first?, ~name, ~owner) >>= handle_result;
};

let handler = (req: Now_lambda.now_proxy_request, _ctx) => {
  let usage = "Usage: github-stars.anmonteiro.now.sh/?repo=username/repo";
  let uri = Uri.of_string(req.path);
  switch (Uri.(get_query_param(uri, "repo"))) {
  | Some(repo) =>
    switch (String.split_on_char('/', repo)) {
    | [owner, name] =>
      handle(~first=?Uri.get_query_param(uri, "first"), ~owner, ~name)
    | _ => send_error_response(usage)
    }
  | None => send_error_response(usage)
  };
};

let () = Now_lambda.io_lambda(handler);
