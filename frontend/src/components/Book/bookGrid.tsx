import { DataGrid } from '@material-ui/data-grid';
import axios from "../../../node_modules/axios";
import React, { Component } from "react";

interface BookData {
  id: number;
  title: string;
  author: string;
}

interface State {
  books: BookData[];
}


const columns = [
  { field: 'id', headerName: 'Id', width: 70 },
  { field: 'title', headerName: 'Title', width: 130 },
  { field: 'author', headerName: 'Author', width: 130},
];


class BookGrid extends Component<{},State> {
  constructor(props:{}) {
    super(props);
    this.state = {books:[]};
  }

  async componentDidMount() {

      await axios.get<BookData[]>("http://localhost:8000/books")
        .then(res => {
          this.setState({books:res.data})
         });
  }

 
  render() {

    return (
      <div style={{height: 400, width: '100%', color: 'whitesmoke' }}>
        <DataGrid rows={this.state.books} columns={columns} pageSize={5} />
      </div>
    );
   }
  }

export default BookGrid;